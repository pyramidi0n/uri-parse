(in-package :cl-user)
(defpackage uri-parse
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match
   :uri-match)
  (:export
   :parse))
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

;; ------------------------------------------------------------------------------

(declaim (inline r-parse-authority))
(declaim (inline r-parse-hier-part))
(declaim (inline r-parse-uri))

(defrule r-parse-authority
    (concatenation (capture *userinfo-lower*
                            *userinfo-upper*
                            (optional-sequence r-userinfo
                                               (terminal +#\@+)))
                   (capture *host-lower*
                            *host-upper*
                            r-host)
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
                   (capture *query-lower*
                            *query-upper*
                            (optional-sequence (terminal +#\?+)
                                               r-query))
                   (capture *fragment-lower*
                            *fragment-upper*
                            (optional-sequence (terminal +#\#+)
                                               r-fragment))))

(defun parse (octets lower upper &key (result-type 'string))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (when (r-parse-uri octets lower upper)
    (let ((scheme   (when *scheme-lower*   (subseq octets *scheme-lower*   *scheme-upper*)))
          (userinfo (when *userinfo-lower* (subseq octets *userinfo-lower* *userinfo-upper*)))
          (host     (when *host-lower*     (subseq octets *host-lower*     *host-upper*)))
          (port     (when *port-lower*     (subseq octets *port-lower*     *port-upper*)))
          (path     (when *path-lower*     (subseq octets *path-lower*     *path-upper*)))
          (query    (when *query-lower*    (subseq octets *query-lower*    *query-upper*)))
          (fragment (when *fragment-lower* (subseq octets *fragment-lower* *fragment-upper*))))
      (case result-type
        (string (values (when (and scheme   (> (length scheme)   0)) (ascii-code-string scheme))
                        (when (and userinfo (> (length userinfo) 0)) (ascii-code-string userinfo))
                        (when (and host     (> (length host)     0)) (ascii-code-string host))
                        (when (and port     (> (length port)     0)) (ascii-code-string port))
                        (when (and path     (> (length path)     0)) (ascii-code-string path))
                        (when (and query    (> (length query)    0)) (ascii-code-string query))
                        (when (and fragment (> (length fragment) 0)) (ascii-code-string fragment))))
        (otherwise (values
                    (when (> (and scheme   (length scheme))   0) scheme)
                    (when (> (and userinfo (length userinfo)) 0) userinfo)
                    (when (> (and host     (length host))     0) host)
                    (when (> (and port     (length port))     0) port)
                    (when (> (and path     (length path))     0) path)
                    (when (> (and query    (length query))    0) query)
                    (when (> (and fragment (length fragment)) 0) fragment)))))))
