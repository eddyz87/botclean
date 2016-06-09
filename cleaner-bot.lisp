;; should play this game:
;;   https://www.hackerrank.com/challenges/botclean

(defpackage :cleaner-bot
  (:use :common-lisp))

(in-package :cleaner-bot)

(defclass state ()
  ((dirty-position-list :type list
                        :initarg :dirty-position-list
                        :accessor dirty-position-list)
   (bot-position :type cons
                 :accessor bot-position
                 :initarg :bot-position)
   (moves-count :type integer
                :accessor moves-count
                :initform 0
                :initarg :moves-count)))

(defun parse-field (string)
  (let ((line 1)
        (pos 0)
        (dirty-pos-list nil)
        (bot-pos nil))
    (loop for ch across string do
         (incf pos)
         (case ch
           (#\Newline (incf line)
                      (setf pos 0))
           (#\b (setf bot-pos (cons line pos)))
           (#\d (push (cons line pos) dirty-pos-list))))
    (make-instance 'state
                   :dirty-position-list dirty-pos-list
                   :bot-position bot-pos)))

(defclass cleaner-bot-game () ())

(defmethod mcts:possible-actions ((_ cleaner-bot-game) state)
  (loop for i below (length (dirty-position-list state))
     collect i))

(defmethod mcts:next-state ((_ cleaner-bot-game) state action)
  (let* ((next-dirty (nth action (dirty-position-list state)))
         (new-dirty-list (remove next-dirty (dirty-position-list state))))
    (make-instance
     'state
     :dirty-position-list new-dirty-list
     :bot-position next-dirty
     :moves-count (+ (moves-count state)
                     (abs (- (car (bot-position state))
                             (car next-dirty)))
                     (abs (- (cdr (bot-position state))
                             (cdr next-dirty)))
                     1))))

(defmethod mcts:clone-state ((_ cleaner-bot-game) state)
  state)

(defmethod mcts:estimate-state-reward ((game cleaner-bot-game) state)
  (let ((actions (mcts:possible-actions game state)))
    (if (null actions)
        (/ (- 200 (moves-count state))
           40)
        (let ((move (random (length actions))))
          (mcts:estimate-state-reward
           game (mcts:next-state game state move))))))

(defun play (field)
  (let* ((game (make-instance 'cleaner-bot-game))
         (root-state (parse-field field))
         (state root-state))
    (values
     (loop while (dirty-position-list state) collect
          (let* ((action (mcts:select-next-move game state 0.1))
                 (move (nth action (dirty-position-list state))))
            (setf state (mcts:next-state game state action))
            move))
     (mcts:estimate-state-reward game state)
     (moves-count state))))
