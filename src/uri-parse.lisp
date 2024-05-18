(in-package :cl-user)
(defpackage uri-parse
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match
   :uri-match)
  (:export
   :parse-octets
   :parse))
(in-package :uri-parse)

;; ------------------------------------------------------------------------------

(declaim (inline plist))
#+:sbcl (declaim (sb-ext:maybe-inline parse-octets))

(defun plist (scheme userinfo host port path query fragment)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (or (simple-array (unsigned-byte 8) (*))
                     string
                     null)
                 scheme
                 userinfo
                 host
                 port
                 path
                 query
                 fragment))
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
  (declare (type (or (simple-array (unsigned-byte 8) (*))
                     null)
                 octets)
           (type fixnum lower)
           (type fixnum upper)
           (type (or symbol null) result-type)
           (type boolean plist))
  (let ((scheme-lower nil)
        (scheme-upper nil)
        (userinfo-lower nil)
        (userinfo-upper nil)
        (host-lower nil)
        (host-upper nil)
        (port-lower nil)
        (port-upper nil)
        (path-lower nil)
        (path-upper nil)
        (query-lower nil)
        (query-upper nil)
        (fragment-lower nil)
        (fragment-upper nil))
    (declare (type matched
                   scheme-lower
                   scheme-upper
                   userinfo-lower
                   userinfo-upper
                   host-lower
                   host-upper
                   port-lower
                   port-upper
                   path-lower
                   path-upper
                   query-lower
                   query-upper
                   fragment-lower
                   fragment-upper))
    (letrules ((r-parse-authority
                (concatenation
                 ;; If we just capture r-userinfo, we'll end up with partial matches stored
                 ;; in the capture variables, even if there's no terminating @. This yields
                 ;; erroneous results.
                 (capture userinfo-lower
                          userinfo-upper
                          (optional-sequence r-userinfo
                                             (terminal +#\@+)))
                 (capture host-lower
                          host-upper
                          r-host)
                 ;; However, in this case, we can just capture the trailing rule in the
                 ;; optional sequence, because if the leading terminal does not exist,
                 ;; the capture will never begin in the first place.
                 (optional-sequence (terminal +#\:+)
                                    (capture port-lower
                                             port-upper
                                             r-port))))
               (r-parse-hier-part
                (alternatives (concatenation (terminal +#\/+)
                                             (terminal +#\/+)
                                             r-parse-authority
                                             (capture path-lower
                                                      path-upper
                                                      r-path-abempty))
                              (capture path-lower
                                       path-upper
                                       r-path-absolute)
                              (capture path-lower
                                       path-upper
                                       r-path-rootless)
                              (capture path-lower
                                       path-upper
                                       r-path-empty)))

               (r-parse-uri
                (concatenation (capture scheme-lower
                                        scheme-upper
                                        r-scheme)
                               (terminal +#\:+)
                               r-parse-hier-part
                               (optional-sequence (terminal +#\?+)
                                                  (capture query-lower
                                                           query-upper
                                                           r-query))
                               (optional-sequence (terminal +#\#+)
                                                  (capture fragment-lower
                                                           fragment-upper
                                                           r-fragment)))))
              (declare (inline r-parse-authority
                               r-parse-hier-part
                               r-parse-uri))
              (labels ((trailing-@-p (octets)
                         (declare (type (simple-array (unsigned-byte 8) (*)) octets))
                         (= (elt octets (1- (length octets))) +#\@+))
                       (ipv6-literal-p (octets)
                         (declare (type (simple-array (unsigned-byte 8) (*)) octets))
                         (and (= (elt octets 0) +#\[+)
                              (= (elt octets (1- (length octets))) +#\]+)))
                       (trim-userinfo (userinfo)
                         (declare (type (simple-array (unsigned-byte 8) (*)) userinfo))
                         (if (trailing-@-p userinfo)
                             (subseq userinfo 0 (1- (length userinfo)))
                             userinfo))
                       (trim-host (host)
                         (declare (type (simple-array (unsigned-byte 8) (*)) host))
                         (if (ipv6-literal-p host)
                             (subseq host 1 (1- (length host)))
                             host)))
                (declare (inline trailing-@-p
                                 ipv6-literal-p
                                 trim-userinfo
                                 trim-host))
                (let ((parsed (r-parse-uri octets lower upper)))
                  (declare (type matched parsed))
                  (when (and parsed (= parsed (length octets)))
                    (let ((scheme   (when scheme-lower   (subseq octets scheme-lower   scheme-upper)))
                          (userinfo (when userinfo-lower (subseq octets userinfo-lower userinfo-upper)))
                          (host     (when host-lower     (subseq octets host-lower     host-upper)))
                          (port     (when port-lower     (subseq octets port-lower     port-upper)))
                          (path     (when path-lower     (subseq octets path-lower     path-upper)))
                          (query    (when query-lower    (subseq octets query-lower    query-upper)))
                          (fragment (when fragment-lower (subseq octets fragment-lower fragment-upper))))
                      (declare (type (or (simple-array (unsigned-byte 8) (*))
                                         null)
                                     scheme
                                     userinfo
                                     host
                                     port
                                     path
                                     query
                                     fragment))
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
                                  (declare (type (or string null)
                                                 scheme-str
                                                 userinfo-str
                                                 host-str
                                                 port-str
                                                 path-str
                                                 query-str
                                                 fragment-str))
                                  (if plist
                                      (plist scheme-str userinfo-str host-str port-str path-str query-str fragment-str)
                                      (values scheme-str userinfo-str host-str port-str path-str query-str fragment-str))))
                        (otherwise (if plist
                                       (plist scheme userinfo host port path query fragment)
                                       (values scheme userinfo host port path query fragment)))))))))))

(defun parse (str &key plist)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type string str)
           (type boolean plist))
  #+:sbcl (declare (inline parse-octets))
  (handler-case
      (let ((octets (ascii-string-code '(simple-array (unsigned-byte 8) (*)) str)))
        (declare (type (simple-array (unsigned-byte 8) (*)) octets))
        (multiple-value-bind (scheme userinfo host port path query fragment)
            (parse-octets octets 0 (length octets))
          (if plist
              (plist scheme userinfo host port path query fragment)
              (values scheme userinfo host port path query fragment))))
    (type-error ())))
