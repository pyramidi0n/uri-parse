(in-package :cl-user)
(defpackage uri-parse.tests
  (:use
   :cl
   :uri-parse)
  (:export
   :test))
(in-package :uri-parse.tests)

;; ------------------------------------------------------------------------------

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
  (assert
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

  t)
