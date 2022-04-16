(in-package :cl-user)
(defpackage uri-parse
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match
   :uri-match)
  (:export
   :parse-octets
   :parse
   :test))
(in-package :uri-parse)

;; ------------------------------------------------------------------------------

(declaim (type matched *scheme-lower*))
(declaim (type matched *scheme-upper*))
(declaim (type matched *userinfo-lower*))
(declaim (type matched *userinfo-upper*))
(declaim (type matched *host-lower*))
(declaim (type matched *host-upper*))
(declaim (type matched *port-lower*))
(declaim (type matched *port-upper*))
(declaim (type matched *path-lower*))
(declaim (type matched *path-upper*))
(declaim (type matched *query-lower*))
(declaim (type matched *query-upper*))
(declaim (type matched *fragment-lower*))
(declaim (type matched *fragment-upper*))

(defparameter *scheme-lower* nil)
(defparameter *scheme-upper* nil)
(defparameter *userinfo-lower* nil)
(defparameter *userinfo-upper* nil)
(defparameter *host-lower* nil)
(defparameter *host-upper* nil)
(defparameter *port-lower* nil)
(defparameter *port-upper* nil)
(defparameter *path-lower* nil)
(defparameter *path-upper* nil)
(defparameter *query-lower* nil)
(defparameter *query-upper* nil)
(defparameter *fragment-lower* nil)
(defparameter *fragment-upper* nil)

(defun clear-captured ()
  (setf *scheme-lower* nil
        *scheme-upper* nil
        *userinfo-lower* nil
        *userinfo-upper* nil
        *host-lower* nil
        *host-upper* nil
        *port-lower* nil
        *port-upper* nil
        *path-lower* nil
        *path-upper* nil
        *query-lower* nil
        *query-upper* nil
        *fragment-lower* nil
        *fragment-upper* nil))

;; ------------------------------------------------------------------------------

(declaim (inline r-parse-authority))
(declaim (inline r-parse-hier-part))
(declaim (inline r-parse-uri))
;; (declaim (inline r-parse-relative-part))
;; (declaim (inline r-parse-relative-ref))
;; (declaim (inline r-parse-uri-reference))
(declaim (inline plist))

(defrule r-parse-authority
    (concatenation
     ;; If we just capture r-userinfo, we'll end up with partial matches stored
     ;; in the capture variables, even if there's no terminating @. This yields
     ;; erroneous results.
     (capture *userinfo-lower*
              *userinfo-upper*
              (optional-sequence r-userinfo
                                 (terminal +#\@+)))
     (capture *host-lower*
              *host-upper*
              r-host)
     ;; However, in this case, we can just capture the trailing rule in the
     ;; optional sequence, because if the leading terminal does not exist,
     ;; the capture will never begin in the first place.
     (optional-sequence (terminal +#\:+)
                        (capture *port-lower*
                                 *port-upper*
                                 r-port))))

(defrule r-parse-hier-part
    (alternatives (concatenation (terminal +#\/+)
                                 (terminal +#\/+)
                                 r-parse-authority
                                 (capture *path-lower*
                                          *path-upper*
                                          r-path-abempty))
                  (capture *path-lower*
                           *path-upper*
                           r-path-absolute)
                  (capture *path-lower*
                           *path-upper*
                           r-path-rootless)
                  (capture *path-lower*
                           *path-upper*
                           r-path-empty)))

(defrule r-parse-uri
    (concatenation (capture *scheme-lower*
                            *scheme-upper*
                            r-scheme)
                   (terminal +#\:+)
                   r-parse-hier-part
                   (optional-sequence (terminal +#\?+)
                                      (capture *query-lower*
                                               *query-upper*
                                               r-query))
                   (optional-sequence (terminal +#\#+)
                                      (capture *fragment-lower*
                                               *fragment-upper*
                                               r-fragment))))

;; (defrule r-parse-relative-part
;;     (alternatives (concatenation (terminal +#\/+)
;;                                  (terminal +#\/+)
;;                                  r-parse-authority
;;                                  (capture *path-lower*
;;                                           *path-upper*
;;                                           r-path-abempty))
;;                   (capture *path-lower*
;;                            *path-upper*
;;                            r-path-absolute)
;;                   (capture *path-lower*
;;                            *path-upper*
;;                            r-path-noscheme)
;;                   (capture *path-lower*
;;                            *path-upper*
;;                            r-path-empty)))

;; (defrule r-parse-relative-ref
;;     (concatenation r-parse-relative-part
;;                    (optional-sequence (terminal +#\?+)
;;                                       (capture *query-lower*
;;                                                *query-upper*
;;                                                r-query))
;;                    (optional-sequence (terminal +#\#+)
;;                                       (capture *fragment-lower*
;;                                                *fragment-upper*
;;                                                r-fragment))))

;; (defrule r-parse-uri-reference
;;     (alternatives r-parse-uri
;;                   r-parse-relative-ref))

(defun plist (scheme userinfo host port path query fragment)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (when (or scheme userinfo host port path query fragment)
    (list :scheme scheme
          :userinfo userinfo
          :host host
          :port port
          :path path
          :query query
          :fragment fragment)))

(defun parse-octets (octets lower upper &key (result-type 'string) plist)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (labels ((trailing-@-p (octets)
             (= (elt octets (1- (length octets))) +#\@+))
           (ipv6-literal-p (octets)
             (and (= (elt octets 0) +#\[+)
                  (= (elt octets (1- (length octets))) +#\]+)))
           (trim-userinfo (userinfo)
             (if (trailing-@-p userinfo)
                 (subseq userinfo 0 (1- (length userinfo)))
                 userinfo))
           (trim-host (host)
             (if (ipv6-literal-p host)
                 (subseq host 1 (1- (length host)))
                 host)))
    (let ((parsed (r-parse-uri octets lower upper)))
      (multiple-value-prog1
          (when (and parsed (= parsed (length octets)))
            (let ((scheme   (when *scheme-lower*   (subseq octets *scheme-lower*   *scheme-upper*)))
                  (userinfo (when *userinfo-lower* (subseq octets *userinfo-lower* *userinfo-upper*)))
                  (host     (when *host-lower*     (subseq octets *host-lower*     *host-upper*)))
                  (port     (when *port-lower*     (subseq octets *port-lower*     *port-upper*)))
                  (path     (when *path-lower*     (subseq octets *path-lower*     *path-upper*)))
                  (query    (when *query-lower*    (subseq octets *query-lower*    *query-upper*)))
                  (fragment (when *fragment-lower* (subseq octets *fragment-lower* *fragment-upper*))))
              (setf scheme   (when (and scheme   (> (length scheme)   0)) scheme)
                    userinfo (when (and userinfo (> (length userinfo) 0)) (trim-userinfo userinfo))
                    host     (when (and host     (> (length host)     0)) (trim-host host))
                    port     (when (and port     (> (length port)     0)) port)
                    path     (when (and path     (> (length path)     0)) path)
                    query    (when (and query    (> (length query)    0)) query)
                    fragment (when (and fragment (> (length fragment) 0)) fragment))
              (case result-type
                (string (let ((scheme-str   (when scheme   (ascii-code-string scheme)))
                              (userinfo-str (when userinfo (ascii-code-string userinfo)))
                              (host-str     (when host     (ascii-code-string host)))
                              (port-str     (when port     (ascii-code-string port)))
                              (path-str     (when path     (ascii-code-string path)))
                              (query-str    (when query    (ascii-code-string query)))
                              (fragment-str (when fragment (ascii-code-string fragment))))
                          (if plist
                              (plist scheme-str userinfo-str host-str port-str path-str query-str fragment-str)
                              (values scheme-str userinfo-str host-str port-str path-str query-str fragment-str))))
                (otherwise (if plist
                               (plist scheme userinfo host port path query fragment)
                               (values scheme userinfo host port path query fragment))))))
        (clear-captured)))))

(defun parse (str &key plist)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type string str))
  (declare (type boolean plist))
  (let ((octets (ascii-string-code '(simple-array (unsigned-byte 8) (*)) str)))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets))
    (multiple-value-bind (scheme userinfo host port path query fragment)
        (parse-octets octets 0 (length octets))
      (if plist
          (plist scheme userinfo host port path query fragment)
          (values scheme userinfo host port path query fragment)))))

(defmacro uri= (form &key scheme userinfo host port path query fragment)
  `(multiple-value-bind (f-scheme f-userinfo f-host f-port f-path f-query f-fragment)
       ,form
     (and (string= ,scheme f-scheme)
          (string= ,userinfo f-userinfo)
          (string= ,host f-host)
          (string= ,port f-port)
          (string= ,path f-path)
          (string= ,query f-query)
          (string= ,fragment f-fragment))))

(defun test ()
  (every #'identity
         (list (uri= (parse "uri:") :scheme "uri")
               (uri= (parse "://"))
               (uri= (parse "//"))
               (uri= (parse "/"))
               (uri= (parse "#"))
               (uri= (parse "?"))
               (uri= (parse "//@"))
               (uri= (parse "@"))
               (uri= (parse "uri://user:pass@example.com:123/one/two.three?q1=a1&q2=a2#body")
                     :scheme "uri"
                     :userinfo "user:pass"
                     :host "example.com"
                     :port "123"
                     :path "/one/two.three"
                     :query "q1=a1&q2=a2"
                     :fragment "body")
               (uri= (parse "uri://10.10.10.10.example.com/en/process")
                     :scheme "uri"
                     :host "10.10.10.10.example.com"
                     :path "/en/process")
               (uri= (parse "uri://10.10.10.10")
                     :scheme "uri"
                     :host "10.10.10.10")
               (uri= (parse "uri://[10.10.10.10]"))
               (uri= (parse "uri://[2606:2800:220:1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "2606:2800:220:1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[::2800:220:1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "::2800:220:1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[::220:1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "::220:1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[100a::220:1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "100a::220:1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[100a:100a::220:1:248:1893:25c8:1946]/test"))
               (uri= (parse "uri://[::1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "::1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[100a::1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "100a::1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[100a:100a::1:248:1893:25c8:1946]/test")
                     :scheme "uri"
                     :host "100a:100a::1:248:1893:25c8:1946"
                     :path "/test")
               (uri= (parse "uri://[100a:100a:100a::1:248:1893:25c8:1946]/test"))
               (uri= (parse "uri://[2001:db8::7]")
                     :scheme "uri"
                     :host "2001:db8::7")
               (uri= (parse "uri://[::ffff:129.144.52.38]")
                     :scheme "uri"
                     :host "::ffff:129.144.52.38")
               (uri= (parse "uri://[2001:db8::1]:80")
                     :scheme "uri"
                     :host "2001:db8::1"
                     :port "80"))))
