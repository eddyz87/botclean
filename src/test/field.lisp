(defpackage :src/test/field
  (:use :common-lisp :src/utils :src/field
        :lisp-unit :cl-quickcheck))

(in-package :src/test/field)

(defun test-put!-get (field-class w h pos &key clone)
  (let ((field (make-instance field-class
                              :width w :height h)))
    (put-cell! field pos :new-value)
    (when clone
      (setf field (clone-field field)))
    (assert-eq (get-cell field pos)
               :new-value)))

(defun in-range-generator (range)
  (lambda () (random range)))

(defun test-put!-get-random (field-class w h)
  (let ((field (make-instance field-class
                              :width w :height h)))
    (assert-true
     (quickcheck
       (for-all ((x (in-range-generator h))
                 (y (in-range-generator w)))
         (put-cell! field (make-pos :x x :y y) :new-value)
         (is eq (get-cell field (make-pos :x x :y y))
             :new-value))))))

(define-test put!.get.array.1
  (test-put!-get 'array-field 10 10 (make-pos :x 3 :y 4)))

(define-test put!.get.clone.array.1
  (test-put!-get 'array-field 10 10 (make-pos :x 3 :y 4)
                 :clone t))

(define-test put!.get.array.random.1
  (test-put!-get-random 'array-field 10 10))


