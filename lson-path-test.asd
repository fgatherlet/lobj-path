#|
  This file is a part of lson-path project.
  Copyright (c) 2019 fgatherlet (fgatherlet@gmail.com)
|#

(defsystem "lson-path-test"
  :defsystem-depends-on ("prove-asdf")
  :author "fgatherlet"
  :license "MIT"
  :depends-on ("lson-path"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "test"))))
  :description "Test system for lson-path"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
