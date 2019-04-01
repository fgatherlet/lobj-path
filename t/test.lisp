(defpackage lson-path-test
  (:use
   :cl
   :lson-path
   :series
   :prove))
(in-package :lson-path-test)

;; (ql:quickload '(:lson-path :prove))

;; NOTE: To run this test file, execute `(asdf:test-system :lson-path)' in your Lisp.

(plan nil)

(defun mkhash (&rest kvs &aux (result (make-hash-table :test 'equal)))
  (iterate (((k v) (chunk 2 2 (scan kvs))))
    (setf (gethash k result) v))
  result)

(defvar lson nil)

(subtest "basic"
  (setq lson
        '(:obj
          ("a" . (:obj
                  ("b" . (:obj
                          ("c" . "d")))))))
  (is (lpath lson "a" "b" "c") "d"))

(subtest "repetition"
  (setq lson
        '(:obj
          ("a" . (:obj
                  ("b" . "c")
                  ("x" . (1 2 3))))))
  (is (lpath lson "a" "x" 1) 2)
  (is (lpath lson (:* :{.}) "b") "c")
  (is (lpath lson (:* :.) "b") "c"))


(subtest "repetition1"
  (setq lson
        '(:obj
          ("a" . (:obj
                  ("b" . "c")
                  ("x" . (1 
                          (:obj ("A" . :a))
                          3))))))
  (is (lpath lson (:* :{.}) "b") "c")
  (is (lpath lson (:* :.) "A") :a))

(subtest "repetition2"
  (setq lson
        '(:obj
          ("a" . (:obj
                  ("b" . "c")
                  ("a" . (:obj
                          ("a" . "x")
                          ("k" . "K")))))))
  
  (is (lpath lson
             '(:{} 3 3 "a"))
      "x")
  
  (is (lpath lson
             (:{} 0 nil :{.})
             "k")
      "K")
  
  (is (lpath lson
             (:* :{.})
             "k")
      "K")
  )

(subtest "lambda"
  (setq lson
        '(:obj
          ("a" . (:obj
                  ("b" . "c")
                  ("x" . (1 
                          (:obj ("A" . :a))
                          3))))))
  (is (lpath lson
              :.
              (lambda (x xp)
                (declare (ignorable xp))
                (and (lson-obj-p x)
                     (lson-obj-ref x "x")))
              2)
      3))

(subtest "ppcre"
  (setq lson
        '(:obj
          ("aaa" . "bbb")
          ("kkk" . "ccc")
          ("naka" . "tani")))

  (is (lpath lson
             (:regex "^na"))
      "tani")
  )

(subtest "or"
  (setq lson
        '(:obj
          ("mmx" . "dummy")
          ("mmm" . (:obj
                    ("aaa" . "xxx")))
          ("kkk" . "ccc")
          ("naka" . "tani")))

  (is (lpath lson
             (:or "aaa"
                  (:seq "mmm" "aaa")))
      "xxx")
  )

(subtest "setf"
  (setq lson
        '(:obj
          ("mmx" . "dummy")
          ("mmm" . (:obj
                    ("aaa" . "xxx")))
          ("kkk" . "ccc")
          ("naka" . "tani")))

  (setf (lpath lson (:* :.) "aaa") "xxx-mod")
  (is (lpath lson "mmm" "aaa") "xxx-mod")
  (is-error (setf (lpath lson "mmm" :.) 100) 'error)
  )


(subtest "hash,array"
  (setq lson
        (mkhash
         "a" (mkhash
              "b" "c"
              "x" #(1 2 3))))
  (is (lpath lson "a" "x" 1) 2)
  (is (lpath lson (:* :{.}) "b") "c")
  (is (lpath lson (:* :.) "b") "c")
  
  (setf (lpath lson (:* :.) "b") "c-mod")
  (is (lpath lson (:* :.) "b") "c-mod")
  
  (setf (lpath lson "a" "x" 1) :two)
  (is (lpath lson "a" "x" 1) :two)
  )

(finalize)
