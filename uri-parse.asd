(defsystem "uri-parse"
  :version "1.0.0"
  :license "BSD-2"
  :description "A fast, RFC-compliant URI validator and parser."
  :author "Stephen Youts"
  :depends-on ("trivial-us-ascii"
               "abnf-match")
  :components
  ((:static-file "LICENSE")
   (:static-file "README.md")
   (:module "src"
    :components ((:file "uri-match")
                 (:file "uri-parse" :depends-on ("uri-match")))))
  :in-order-to ((test-op (test-op "uri-parse/tests"))))

(defsystem "uri-parse/tests"
  :author "Stephen Youts"
  :license "BSD-2"
  :description "Tests for uri-parse."
  :depends-on ("uri-parse")
  :components ((:module "tests"
                :components
                ((:file "uri-parse-tests"))))
  :perform (test-op (op c) (symbol-call :uri-parse.tests :test)))
