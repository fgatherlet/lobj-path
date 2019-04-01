(defpackage lson-path
  (:nicknames
   :lpath)
  (:export
   :lpath

   :lson-obj-p
   :lson-seq-p
   :lson-obj-ref
   :lson-seq-ref
   )
  (:use
   :cl
   :series
   )
  (:import-from
   :let-over-lambda
   :defmacro!
   )
  (:import-from
   :alexandria
   :when-let
   ))
