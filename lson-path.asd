#|
  This file is a part of lson-path project.
  Copyright (c) 2019 fgatherlet (fgatherlet@gmail.com)
|#

#|
  Author: fgatherlet (fgatherlet@gmail.com)
|#

(defsystem "lson-path"
  :version "0.1.0"
  :author "fgatherlet"
  :license "MIT"
  :depends-on ("series"
               "let-over-lambda"
               "alexandria"
               "jsown"
               "cl-ppcre"
               )
  :components ((:module "src"
                :components
                (
                 (:file "api" :depends-on ("matcher"))
                 (:file "matcher" :depends-on ("base"))
                 (:file "base" :depends-on ("package"))
                 (:file "package")
                 )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "lson-path-test"))))
