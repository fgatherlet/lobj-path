(in-package :lson-path)

(defvar *repetition-depth* -1)
(defvar *value-to-set* nil)

;;; ------------------------------

(defun jsown-object-p (x)
  (and (consp x)
       (eql :obj (car x))))
(deftype jsown-object ()
  `(satisfies jsown-object-p))

(defun lson-obj-p (x)
  (or (jsown-object-p x)
      (hash-table-p x)))
(deftype lson-obj ()
  `(satisfies lson-obj-p))


(defun lson-obj-ref (x path)
  (typecase x
    (jsown-object
     (handler-case
         (jsown:val x path)
       (error (e)
         (declare (ignore e))
         nil)
       ))
    (hash-table
     (gethash path x))))

(defun (setf lson-obj-ref) (y x path)
  (typecase x
    (hash-table
     (setf (gethash path x) y))
    (jsown-object
     (setf (jsown:val x path) y))))

(defun lson-obj-keys (x)
  (typecase x
    (hash-table
     (collect (scan-hash x)))
    (jsown-object
     (mapcar #'car (cdr x)))))

;;; ------------------------------

(defun lson-seq-p (x)
  (and (not (lson-obj-p x))
       (or (listp x)
           (vectorp x))))
(deftype lson-seq ()
  `(satisfies lson-seq-p))

(defun lson-seq-ref (x path)
  (typecase x
    (list
     (and (integerp path)
          (< path (length x))
          (nth path x)))
    (vector
     (and (integerp path)
          (< path (length x))
          (aref x path)))))

(defun (setf lson-seq-ref) (y x path)
  (typecase x
    (list
     (and (integerp path)
          (< path (length x))
          (setf (nth path x) y)))
    (vector
     (and (integerp path)
          (< path (length x))
          (setf (aref x path) y)))))
