(in-package :lson-path)

(defgeneric compile-path (path cont))

;;; ------------------------------ set

;; xp is x's parent.
;; xc is x's child.

(defmethod compile-path ((path string) (cont function))
  (lambda (x xp)
    (declare (ignore xp))
    (when-let ((xc (lson-obj-ref x path)))
      (funcall cont xc x))))

(defmethod compile-path ((path integer) (cont function))
  (lambda (x xp)
    (declare (ignore xp))
    (when-let ((xc (lson-seq-ref x path)))
      (funcall cont xc x))))

(defmethod compile-path ((path (eql :{.})) cont)
  "hash wildcard"
  (check-type cont function)
  (lambda (x xp)
    (declare (ignore xp))
    (or (and (lson-obj-p x)
             (collect-first
              (choose-if
               #'identity
               (mapping ((lson-obj-key (scan (lson-obj-keys x))))
                 (funcall cont (lson-obj-ref x lson-obj-key) x))))))))

(defmethod compile-path ((path (eql :[.])) cont)
  "wildcard"
  (check-type cont function)
  (lambda (x xp)
    (declare (ignore xp))
    (or (and (lson-seq-p x)
             (collect-first
              (choose-if
               #'identity
               (mapping ((lson-obj-val (scan x)))
                 (funcall cont lson-obj-val x))))))))

(defmethod compile-path ((path function) cont)
  (check-type cont function)
  (lambda (x xp &aux
                  (xcs (multiple-value-list (funcall path x xp))))
    ;;(declare (ignore xp))
    (collect-first
     (choose-if
      #'identity
      (mapping ((xc (scan xcs)))
        (when xc
          (funcall cont xc x)))))))

(defun compile-path-{} (start-end-path cont)
  (check-type cont function)
  (destructuring-bind (start end path) start-end-path
    (let (repeat)
      (flet ((walk (x xp)
               (let ((*repetition-depth* (1+ *repetition-depth*)))
                 (cond
                   ((< *repetition-depth* start)
                    (funcall repeat x xp))
                   ((and end (< end *repetition-depth*))) ;; do nothing
                   (t (or (funcall cont x xp)
                          (funcall repeat x xp)))))))
        (setq repeat (compile-path path #'walk))
        (lambda (x xp)
          (let ((*repetition-depth* -1))
            (walk x xp)))))))

;;; ------------------------------ setf
(defmethod compile-path ((path string) (cont (eql :set)))
  (lambda (x xp)
    (declare (ignore xp))
    (when-let ((xc (lson-obj-ref x path)))
      ;;(when (lson-obj-p x)
      (setf (lson-obj-ref x path) *value-to-set*)
      (values (lson-obj-ref x path) x))))

(defmethod compile-path ((path integer) (cont (eql :set)))
  (lambda (x xp)
    (declare (ignore xp))
    (when-let ((xc (lson-seq-ref x path)))
      (setf (lson-seq-ref x path) *value-to-set*)
      (values (lson-seq-ref x path) x))))

;;; ------------------------------
(defmethod compile-path ((path list) cont)
  (ecase (car path)
    ('cl:lambda ;; ugly
        (compile-path (eval path) cont))
    (:regex
     (let ((scanner (cadr path))) ;; TODO: create scanner.
       (compile-path
        (lambda (x xp)
          (declare (ignore xp))
          (when (lson-obj-p x)
            (apply #'values
                   (collect
                       (mapping ((k (choose-if
                                     (lambda (k)
                                       (ppcre:scan scanner k))
                                     (scan (lson-obj-keys x)))))
                         (lson-obj-ref x k))))))
        cont)))
    (:seq
     (compile-path-seq (cdr path) cont))
    (:{} ;; repetition
     (compile-path-{} (cdr path) cont))
    (:*  ;; repetition 0 to nil
     (compile-path-{} (list* 0 nil (cdr path)) cont))
    (:+  ;; repetition 1 to nil
     (compile-path-{} (list* 1 nil (cdr path)) cont))
    (:?  ;; repetition 0 to 1
     (compile-path-{} (list* 0 1 (cdr path)) cont))
    (:or
     (compile-path-or (cdr path) cont))
    ))

(defun compile-path-seq (paths cont)
  (labels ((make (cont rpaths)
             (let ((path (car rpaths)))
               (unless path (return-from make cont))
               (let ((next-cont (compile-path path cont)))
                 (make next-cont (cdr rpaths))))))
    (make cont (reverse paths))))

(defun compile-path-or (paths cont)
  (let ((compiled-paths (mapcar (lambda (path)
                                 (compile-path path cont))
                               paths)))
    (lambda (x xp)
      (block top
        (dolist (compiled-path compiled-paths)
          (when-let ((xc (funcall compiled-path x xp)))
            (return-from top (values xc x))))))))


(defmethod compile-path ((path (eql :.)) cont)
  "shorthand"
  (compile-path '(:or :{.} :[.]) cont))

