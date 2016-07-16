(defpackage :src/path
  (:nicknames :path)
  (:use :common-lisp
        :src/utils
        :src/field)
  (:import-from :alexandria)
  (:import-from :cl-heap)
  (:export #:a-star-find-path
           #:path-to-moves
           #:cell-is-free))

(in-package :src/path)

(defclass info ()
  ((prev-pos :initarg :prev-pos
             :reader prev-pos)
   (path-len :initarg :path-len
             :reader path-len)))

(defvar *print-steps* nil)

(define-condition path-not-found (error)
  ())

;;

(defun cell-is-free (cell)
  (not (eq cell :wall)))

;;

(defun a-star-find-path (field pos-from pos-to)
  (let ((queue (make-instance 'cl-heap:priority-queue))
        (start-info (make-instance 'info
                                   :prev-pos nil
                                   :path-len 0))
        (info-field (make-instance 'array-field
                                   :width (width field)
                                   :height (height field))))
    (cl-heap:enqueue queue pos-from (estimate pos-from pos-to))
    (put-cell! info-field pos-from start-info)
    (handler-case
        (progn
          (loop until (one-step queue field info-field pos-to)
             for step-num from 1
             finally (when *print-steps*
                       (format t "Num steps = ~A~%" step-num)))
          (read-path info-field pos-to))
      (path-not-found ()
        nil))))

(defun estimate (pos-from pos-to)
  (+ (abs (- (pos-x pos-from)
             (pos-x pos-to)))
     (abs (- (pos-y pos-from)
             (pos-y pos-to)))))

(defun one-step (queue field info-field pos-to)
  (labels ((%try-put (pos prev len)
             (when (and (>= (pos-x pos) 0)
                        (>= (pos-y pos) 0)
                        (< (pos-x pos) (width field))
                        (< (pos-y pos) (height field)))
               (let ((cell (get-cell field pos))
                     (info (get-cell info-field pos)))
                 (when (and (cell-is-free cell)
                            (or (null info)
                                (< len (path-len info))))
                   (put-cell! info-field pos
                              (make-instance 'info
                                             :prev-pos prev
                                             :path-len len))
                   (cl-heap:enqueue queue pos (+ len (estimate pos pos-to))))))))
    (when (= (cl-heap:queue-size queue)
             0)
      (error 'path-not-found))
    (let* ((new-pos (cl-heap:dequeue queue))
           (info (get-cell info-field new-pos))
           (len (path-len info)))
      (if (equalp new-pos pos-to)
          t
          (progn
            (%try-put (pos-plus new-pos 1 0) new-pos len)
            (%try-put (pos-plus new-pos -1 0) new-pos len)
            (%try-put (pos-plus new-pos 0 1) new-pos len)
            (%try-put (pos-plus new-pos 0 -1) new-pos len)
            nil)))))

(defun read-path (info-field pos-to)
  (labels ((%read (pos accum)
             (let* ((info (get-cell info-field pos))
                    (prev (prev-pos info)))
               (if prev
                   (%read prev (cons prev accum))
                   accum))))
    (%read pos-to (list pos-to))))

(defun two-pos-to-move (pos-from pos-to)
  (if (= (pos-x pos-from) (pos-x pos-to))
      (if (> (pos-y pos-from) (pos-y pos-to))
          :up
          :down)
      (if (> (pos-x pos-from) (pos-x pos-to))
          :left
          :right)))

(defun path-to-moves (path)
  (loop for (pos1 pos2 . _) on path 
     when pos2
     collect
       (two-pos-to-move pos1 pos2)))
