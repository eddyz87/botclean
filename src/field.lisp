(defpackage :src/field
  (:nicknames :field)
  (:use :common-lisp :src/utils)
  (:import-from :alexandria)
  (:export #:get-cell #:put-cell #:put-cell!
           #:array-field
           #:width #:height
           #:make-pos #:pos-x #:pos-y
           #:clone-field
           #:pos-plus))

(in-package :src/field)

(defstruct pos
 x
 y)

(defun pos-plus (pos dx dy)
  (make-pos :x (+ dx (pos-x pos))
            :y (+ dy (pos-y pos))))

(defclass field ()
 ((width :reader width
         :initarg :width)
  (height :reader height
          :initarg :height)))

(defgeneric get-cell (field pos))
;; Mutable
(defgeneric put-cell! (field pos val))
;; Immutable
(defgeneric put-cell (field pos val))

(defgeneric clone-field (field))

(defclass array-field (field)
  ((cells-array :initform nil
                :initarg :cells-array
                :accessor cells-array)))

(defmethod initialize-instance :after ((field array-field) &rest initargs)
  (declare (ignore initargs))
  (with-slots (width height) field
    (setf (cells-array field)
          (make-array (list height width)
                      :initial-element nil))))

(defmethod get-cell ((field array-field) pos)
  (with-slots (cells-array) field
    (aref cells-array (pos-y pos) (pos-x pos))))

(defmethod put-cell! ((field array-field) pos val)
  (with-slots (cells-array) field
    (setf (aref cells-array (pos-y pos) (pos-x pos))
          val)))

(defmethod clone-field ((field array-field))
  (copy-instance
   field
   :cells-array (alexandria:copy-array (cells-array field))))
