(defpackage :src/test/path
  (:use :common-lisp :src/utils :src/field :src/path
        :src/state
        :lisp-unit :cl-quickcheck))

(in-package :src/test/path)

(defun simple-field ()
  (format nil "~{~A~%~}"
          '("-----"
            "-----"
            "-b---"
            "-----"
            "-----"
            "-----")
          ))

(defun shortest-path-len (pos-from pos-to)
  (+ (abs (- (pos-x pos-from)
             (pos-x pos-to)))
     (abs (- (pos-y pos-from)
             (pos-y pos-to)))))

(defun test-find-len (pos-from pos-to)
  (with-input-from-string (stream (simple-field))
    (let ((state (state::parse-field stream 'array-field 'state::mutable-state)))
      (assert-equal (1+ (shortest-path-len pos-from pos-to))
                    (length (a-star-find-path (state::field state)
                                              pos-from
                                              pos-to))))))

(defun test-find-path (pos-from pos-to path)
  (with-input-from-string (stream (simple-field))
    (let ((state (state::parse-field stream 'array-field 'state::mutable-state)))
      (assert-equal path
                    (path-to-moves
                     (a-star-find-path (state::field state)
                                       pos-from
                                       pos-to))))))

(define-test path-len.1
  (test-find-path (make-pos :x 0 :y 0) (make-pos :x 3 :y 0)
                  '(:right :right :right))
  (test-find-len (make-pos :x 0 :y 0) (make-pos :x 3 :y 0))
  (test-find-len (make-pos :x 0 :y 0) (make-pos :x 4 :y 4))
  (test-find-path (make-pos :x 0 :y 0) (make-pos :x 10 :y 10)
                  '()))

