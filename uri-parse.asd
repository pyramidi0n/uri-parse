(defsystem "uri-parse"
  :version "1.0.0"
  :license "GNU Affero General Public License v3"
  :description "A library for fast, rigorous URI syntax validation and parsing."
  :author "Stephen Youts"
  :long-description
  "TODO"
  :depends-on ("trivial-us-ascii"
               "abnf-match")
  :components
  ((:static-file "LICENSE")
   (:static-file "AGPLv3")
   (:static-file "README.md")
   (:module "src"
    :components ((:file "uri-match")
                 (:file "uri-parse" :depends-on ("uri-match"))))))
