(defpackage :src/state
  (:nicknames :state)
  (:use :common-lisp :src/field :src/utils)
  (:import-from :mcts))

(in-package :src/state)

(defclass base-state ()
  ((field :accessor field
          :initarg :field)
   (bot-position :accessor bot-position
                 :initarg :bot-position)
   (moves-count :accessor moves-count
                :initarg :moves-count
                :initform 0)
   (dirty-cells :accessor dirty-cells
                :initarg :dirty-cells)))

(defclass mutable-state (base-state)
  ())

(defgeneric is-mutable-state-class (state))
(defmethod is-mutable-state-class (state)
  t)

(defclass bot-game-mutable-state () ())

(defun parse-field (stream field-class state-class)
  (let* ((lines (loop for line = (read-line stream nil)
                   while line
                   collect line))
         (mutable (is-mutable-state-class state-class))
         (height (length lines))
         (width (length (first lines)))
         (field (make-instance field-class
                               :width width
                               :height height))
         bot-position
         (dirty-cells 0))
    (loop for line in lines
       for y from 0 do
         (loop for ch across line
            for x from 0 do
              (case ch
                (#\b (setf bot-position
                           (make-pos :x x :y y)))
                (#\d (incf dirty-cells)
                     (if mutable
                         (put-cell! field (make-pos :x x :y y) :dirt)
                         (setf field
                               (put-cell field (make-pos :x x :y y) :dirt)))))))
    (make-instance state-class
                   :field field
                   :bot-position bot-position
                   :dirty-cells dirty-cells)))

(defun possible-moves (pos width height)
  (append
   (when (> (pos-x pos)
            0)
     (list :left))
   (when (> (pos-y pos)
            0)
     (list :up))
   (when (< (pos-x pos)
            (1- width))
     (list :right))
   (when (< (pos-y pos)
            (1- height))
     (list :down))))

;; MCTS methods

(defmethod mcts:possible-actions ((game bot-game-mutable-state) state)
  (when (= (dirty-cells state) 0)
    (return-from mcts:possible-actions nil))
  (append (possible-moves (bot-position state)
                          (width (field state))
                          (height (field state)))
          (when (eq (get-cell (field state)
                              (bot-position state))
                    :dirt)
            (list :clean))))

(defmethod mcts:next-state ((game bot-game-mutable-state) state action)
  (if (eq action :clean)
      (progn
        (put-cell! (field state) (bot-position state) nil)
        (decf (dirty-cells state)))
      (multiple-value-bind (dx dy)
          (case action
            (:up (values 0 -1))
            (:down (values 0 1))
            (:left (values -1 0))
            (:right (values 1 0)))
        (setf (bot-position state)
              (make-pos :x (+ (pos-x (bot-position state)) dx)
                        :y (+ (pos-y (bot-position state)) dy)))))
  (incf (moves-count state))
  state)

(defmethod mcts:clone-state ((game bot-game-mutable-state) state)
  (copy-instance state
                 :field (clone-field (field state))))

(defparameter *num-random-estimations* 10)

(defmethod mcts:estimate-state-reward ((game bot-game-mutable-state) state)
  (labels ((%loop (state num-iters)
             (let ((actions (mcts:possible-actions game state)))
               (if (or (null actions) (= num-iters 0))
                   (- (/ (- 200 (moves-count state))
                         40)
                      (* 5 (dirty-cells state)))
                   (let ((move (random (length actions))))
                     (%loop (mcts:next-state game state (nth move actions))
                            (1- num-iters)))))))
    (loop for _ from 1 to *num-random-estimations* maximize
         (%loop (mcts:clone-state game state)
                (* (width (field state))
                   (height (field state)))))))

;;;;;;;;;;;;;;;

(defun visualize-state (state stream)
  (format stream "Moves: ~A~%Dirty cells: ~A~%"
          (moves-count state)
          (dirty-cells state))
  (loop for y from 0 to (1- (height (field state))) do
       (loop for x from 0 to (1- (width (field state))) do
            (let* ((pos (make-pos :x x :y y ))
                   (val (get-cell (field state) pos)))
              (princ (if (equalp pos (bot-position state))
                         #\b
                         (case val
                           (:dirt #\d)
                           (otherwise #\-)))
                     stream)))
       (terpri stream)))

(defun sample-field-1 ()
  (format nil "~{~A~%~}"
          '("----d"
            "--d--"
            "-b-d-"
            "---d-"
            "-dd--"
            "-----")
          ))

(defun sample-field-2 ()
  (format nil "~{~A~%~}"
          '("----d-----------------------"
            "--d--------------------d----"
            "-b-d--------------------dd--"
            "---d------------------------"
            "-dd-------------------------"
            "------------------------dd--"
            "--------------------------d-"
            "----------------------------"
            "----------------------------"
            "----------------------dddd--"
            "--------------------------d-"
            "-----------------------d-d--"
            "------------------------d---"
            "--d-------------------------"
            "----------------------------"
            )))

(defun mcts-play (field-str field-class state-class game-class &key (visualize t) (max-moves 100))
  (let* ((game (make-instance game-class))
         (root-state (with-input-from-string (stream field-str)
                       (parse-field stream field-class state-class))))
    (labels ((%step (state num-moves)
               (when visualize
                 (visualize-state state *standard-output*))
               (if (or (= num-moves 0)
                       (= (dirty-cells state) 0))
                   state
                   (let* ((action (mcts:select-next-move game state 0.5))
                          (new-state (mcts:next-state game state action)))
                     (%step new-state (1- num-moves))))))
      (%step root-state max-moves))))

(defun mutable-mcts-play (field-str &key (visualize t) (max-moves 100))
  (mcts-play field-str 'array-field 'mutable-state 'bot-game-mutable-state
             :visualize visualize
             :max-moves max-moves))
