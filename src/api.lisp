(in-package :lson-path)

(defmacro lpath (lson &rest paths)
  `(funcall #'lpath% ,lson ',paths))

#+nil(defsetf lpath (lson &rest paths) (new-val)
  `(lpath-set% ,lson ',paths ,new-val))

(define-setf-expander lpath (lson &rest paths) ;;&environment env)
  (let ((store (gensym "STORE")))
    (values '()
            '()
            `(,store)
            `(lpath-set% ,lson ',paths ,store)
            `(lpath-get% ,lson ',paths))))

(defun lpath% (lson paths)
  (funcall (compile-path (cons :seq paths) #'values) lson nil))

(defun lpath-set% (lson paths value-to-set)
  (let ((last-path (car (last paths))))
    (unless (typep last-path '(or string integer))
      (error "[setf lpath error] last of paths (~s) must be string or integer." last-path)))
  (let ((*value-to-set* value-to-set))
    (funcall (compile-path (cons :seq paths) :set) lson nil)))

(defsetf lpath% lpath-set%)





