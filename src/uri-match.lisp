(in-package :cl-user)
(defpackage uri-match
  (:use
   :cl
   :trivial-us-ascii
   :abnf-match)
  (:export
   :r-gen-delims
   :r-sub-delims
   :r-unreserved
   :r-pct-encoded
   :r-dec-octet
   :r-h16
   :r-port
   :r-scheme

   :r-reserved
   :r-pchar
   :r-segment-nz-nc
   :r-reg-name
   :r-ipv4-address
   :r-ipvfuture
   :r-userinfo

   :r-query
   :r-fragment
   :r-path-empty
   :r-segment
   :r-segment-nz

   :r-path-noscheme
   :r-path-rootless
   :r-path-absolute
   :r-path-abempty
   :r-ls32

   :r-path
   :r-ipv6-address

   :r-ip-literal

   :r-host

   :r-authority

   :r-relative-part
   :r-hier-part

   :r-relative-ref
   :r-uri
   :r-absolute-uri

   :r-uri-reference))
(in-package :uri-match)

;; ------------------------------------------------------------------------------
;;
;; RFC 3986
;; Uniform Resource Identifier (URI): Generic Syntax
;;
;; This is a complete implementation of the specification.
;;
;; ------------------------------------------------------------------------------

(declaim (inline r-gen-delims))
(declaim (inline r-sub-delims))
(declaim (inline r-unreserved))
(declaim (inline r-pct-encoded))
(declaim (inline r-dec-octet))
(declaim (inline r-h16))
(declaim (inline r-port))
(declaim (inline r-scheme))

(declaim (inline r-reserved))
(declaim (inline r-pchar))
(declaim (inline r-segment-nz-nc))
(declaim (inline r-reg-name))
(declaim (inline r-ipv4-address))
(declaim (inline r-ipvfuture))
(declaim (inline r-userinfo))

(declaim (inline r-query))
(declaim (inline r-fragment))
(declaim (inline r-path-empty))
(declaim (inline r-segment))
(declaim (inline r-segment-nz))

(declaim (inline r-path-noscheme))
(declaim (inline r-path-rootless))
(declaim (inline r-path-absolute))
(declaim (inline r-path-abempty))
(declaim (inline r-ls32))

(declaim (inline r-path))
(declaim (inline r-ipv6-address))

(declaim (inline r-ip-literal))

(declaim (inline r-host))

(declaim (inline r-authority))

(declaim (inline r-relative-part))
(declaim (inline r-hier-part))

(declaim (inline r-relative-ref))
(declaim (inline r-uri))
(declaim (inline r-absolute-uri))

(declaim (inline r-uri-reference))

;; ---

(defrule r-gen-delims
    (alternatives (terminal +#\:+)
                  (terminal +#\/+)
                  (terminal +#\?+)
                  (terminal +#\#+)
                  (terminal +#\[+)
                  (terminal +#\]+)
                  (terminal +#\@+)))

(defrule r-sub-delims
    (alternatives (terminal +#\!+)
                  (terminal +#\$+)
                  (terminal +#\&+)
                  (terminal +#\'+)
                  (terminal +#\(+)
                  (terminal +#\)+)
                  (terminal +#\*+)
                  (terminal +#\++)
                  (terminal +#\,+)
                  (terminal +#\;+)
                  (terminal +#\=+)))

(defrule r-unreserved
    (alternatives r-alpha
                  r-digit
                  (terminal +#\-+)
                  (terminal +#\.+)
                  (terminal +#\_+)
                  (terminal +#\~+)))

(defrule r-pct-encoded
    (concatenation (terminal +#\%+)
                   r-hexdig
                   r-hexdig))

(defrule r-dec-octet
    (alternatives r-digit
                  (concatenation (value-range-alternatives +#\1+ +#\9+)
                                 r-digit)
                  (concatenation (terminal +#\1+)
                                 (specific-repetition 2 r-digit))
                  (concatenation (terminal +#\2+)
                                 (value-range-alternatives +#\0+ +#\4+)
                                 r-digit)
                  (concatenation (terminal +#\2+)
                                 (terminal +#\5+)
                                 (value-range-alternatives +#\0+ +#\5+))))

(defrule r-h16
    (variable-repetition r-hexdig :minimum 1 :maximum 4))

(defrule r-port
    (variable-repetition r-digit))

(defrule r-scheme
    (concatenation r-alpha
                   (variable-repetition (alternatives r-alpha
                                                      r-digit
                                                      (terminal +#\++)
                                                      (terminal +#\-+)
                                                      (terminal +#\.+)))))

;; ---

(defrule r-reserved
    (alternatives r-gen-delims
                  r-sub-delims))

(defrule r-pchar
    (alternatives r-unreserved
                  r-pct-encoded
                  r-sub-delims
                  (terminal +#\:+)
                  (terminal +#\@+)))

(defrule r-segment-nz-nc
    (variable-repetition (alternatives r-unreserved
                                       r-pct-encoded
                                       r-sub-delims
                                       (terminal +#\@+))
                         :minimum 1))

(defrule r-reg-name
    (variable-repetition (alternatives r-unreserved
                                       r-pct-encoded
                                       r-sub-delims)))

(defrule r-ipv4-address
    (concatenation r-dec-octet
                   (terminal +#\.+)
                   r-dec-octet
                   (terminal +#\.+)
                   r-dec-octet
                   (terminal +#\.+)
                   r-dec-octet
                   (terminal +#\.+)))

(defrule r-ipvfuture
    (concatenation (terminal +#\v+)
                   (variable-repetition r-hexdig :minimum 1)
                   (terminal +#\.+)
                   (variable-repetition (alternatives r-unreserved
                                                      r-sub-delims
                                                      (terminal +#\:+))
                                        :minimum 1)))

(defrule r-userinfo
    (variable-repetition (alternatives r-unreserved
                                       r-pct-encoded
                                       r-sub-delims
                                       (terminal +#\:+))))

;; ---

(defrule r-query
    (variable-repetition (alternatives r-pchar
                                       (terminal +#\/+)
                                       (terminal +#\?+))))

(defrule r-fragment
    (variable-repetition (alternatives r-pchar
                                       (terminal +#\/+)
                                       (terminal +#\?+))))

(defrule r-path-empty
    (specific-repetition 0 r-pchar))

(defrule r-segment
    (variable-repetition r-pchar))

(defrule r-segment-nz
    (variable-repetition r-pchar
                         :minimum 1))

;; ---

(defrule r-path-noscheme
    (concatenation r-segment-nz-nc
                   (variable-repetition (concatenation (terminal +#\/+)
                                                       r-segment))))

(defrule r-path-rootless
    (concatenation r-segment-nz
                   (variable-repetition (concatenation (terminal +#\/+)
                                                       r-segment))))

(defrule r-path-absolute
    (concatenation (terminal +#\/+)
                   (optional-sequence r-segment-nz
                                      (variable-repetition (concatenation (terminal +#\/+)
                                                                          r-segment)))))

(defrule r-path-abempty
    (variable-repetition (concatenation (terminal +#\/+)
                                        r-segment)))

(defrule r-ls32
    (alternatives (concatenation r-h16
                                 (terminal +#\:+)
                                 r-h16)
                  r-ipv4-address))

;; ---

(defrule r-path
    (alternatives r-path-abempty
                  r-path-absolute
                  r-path-noscheme
                  r-path-rootless
                  r-path-empty))

(defrule r-ipv6-address
    (alternatives (concatenation (specific-repetition 6
                                                      (concatenation r-h16
                                                                     (terminal +#\:+)))
                                 r-ls32)
                  (concatenation (terminal +#\:+)
                                 (terminal +#\:+)
                                 (specific-repetition 5
                                                      (concatenation r-h16
                                                                     (terminal +#\:+)))
                                 r-ls32)
                  (concatenation (optional-sequence r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+)
                                 (specific-repetition 4
                                                      (concatenation r-h16
                                                                     (terminal +#\:+)))
                                 r-ls32)
                  (concatenation (optional-sequence (variable-repetition (concatenation r-h16
                                                                                        (terminal +#\:+))
                                                                         :maximum 1)
                                                    r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+)
                                 (specific-repetition 3
                                                      (concatenation r-h16
                                                                     (terminal +#\:+)))
                                 r-ls32)
                  (concatenation (optional-sequence (variable-repetition (concatenation r-h16
                                                                                        (terminal +#\:+))
                                                                         :maximum 2)
                                                    r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+)
                                 (specific-repetition 2
                                                      (concatenation r-h16
                                                                     (terminal +#\:+)))
                                 r-ls32)
                  (concatenation (optional-sequence (variable-repetition (concatenation r-h16
                                                                                        (terminal +#\:+))
                                                                         :maximum 3)
                                                    r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+)
                                 r-h16
                                 (terminal +#\:+)
                                 r-ls32)
                  (concatenation (optional-sequence (variable-repetition (concatenation r-h16
                                                                                        (terminal +#\:+))
                                                                         :maximum 4)
                                                    r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+)
                                 r-ls32)
                  (concatenation (optional-sequence (variable-repetition (concatenation r-h16
                                                                                        (terminal +#\:+))
                                                                         :maximum 5)
                                                    r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+)
                                 r-h16)
                  (concatenation (optional-sequence (variable-repetition (concatenation r-h16
                                                                                        (terminal +#\:+))
                                                                         :maximum 6)
                                                    r-h16)
                                 (terminal +#\:+)
                                 (terminal +#\:+))))

;; ---

(defrule r-ip-literal
    (concatenation (terminal +#\[+)
                   (alternatives r-ipv6-address r-ipvfuture)
                   (terminal +#\]+)))

;; ---

(defrule r-host
    (alternatives r-ip-literal r-ipv4-address r-reg-name))

;; ---

(defrule r-authority
    (concatenation (optional-sequence r-userinfo (terminal +#\@+))
                   r-host
                   (optional-sequence (terminal +#\:+) r-port)))

;; ---

(defrule r-relative-part
    (alternatives (concatenation (terminal +#\/+)
                                 (terminal +#\/+)
                                 r-authority
                                 r-path-abempty)
                  r-path-absolute
                  r-path-noscheme
                  r-path-empty))

(defrule r-hier-part
    (alternatives (concatenation (terminal +#\/+)
                                 (terminal +#\/+)
                                 r-authority
                                 r-path-abempty)
                  r-path-absolute
                  r-path-rootless
                  r-path-empty))

;; ---

(defrule r-relative-ref
    (concatenation r-relative-part
                   (optional-sequence (terminal +#\?+)
                                      r-query)
                   (optional-sequence (terminal +#\#+)
                                      r-fragment)))

(defrule r-uri
    (concatenation r-scheme
                   (terminal +#\:+)
                   r-hier-part
                   (optional-sequence (terminal +#\?+)
                                      r-query)
                   (optional-sequence (terminal +#\#+)
                                      r-fragment)))

(defrule r-absolute-uri
    (concatenation r-scheme
                   (terminal +#\:+)
                   r-hier-part
                   (optional-sequence (terminal +#\?+)
                                      r-query)))

;; ---

(defrule r-uri-reference
    (alternatives r-uri
                  r-relative-ref))
