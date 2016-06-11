(defpackage :src/test/state
  (:use :common-lisp :src/field :src/state
        :lisp-unit))

(in-package :src/test/state)

(defun simple-field ()
  (format nil "~{~A~%~}"
          '("----d"
            "--d--"
            "-b-d-"
            "---d-"
            "-dd--"
            "-----")
          ))

(defun test-parse-1 (field-class state-class)
  (with-input-from-string (stream (simple-field))
    (let ((state (state::parse-field stream field-class state-class)))
      (assert-equal 5 (width (state::field state)))
      (assert-equal 6 (height (state::field state)))
      (assert-equalp (make-pos :x 1 :y 2)
                     (state::bot-position state))
      (assert-equal nil (get-cell (state::field state)
                                  (make-pos :x 1 :y 2)))
      (assert-equal :dirt (get-cell (state::field state)
                                    (make-pos :x 2 :y 1))))))

(define-test parse.array.1
  (test-parse-1 'array-field 'state::mutable-state))
