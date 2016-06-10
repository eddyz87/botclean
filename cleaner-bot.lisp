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

(defvar *game*)

(defun brute-force-play (field)
  (let* ((root-state (parse-field field))
         (*game* (make-instance 'cleaner-bot-game))
         (solution (brute-force-step root-state nil nil)))
    (when solution
      (values
       (reverse (second solution))
       (first solution)
       (moves-count (third solution))))))

(defun brute-force-step (state moves current-best)
  (let ((dirty-position-list (dirty-position-list state)))
    (cond
      ((null dirty-position-list)
       (list (mcts:estimate-state-reward *game* state)
             moves
             state))
      (t
       (let ((best current-best))
         (loop
            for i below (length dirty-position-list)
            for move in dirty-position-list
            do
              (let* ((next-state (mcts:next-state *game* state i)))
                (when (or (null best)
                          (< (moves-count next-state)
                             (moves-count (third best))))
                  (let ((next (brute-force-step next-state
                                                (cons move moves)
                                                best)))
                    (when (or (null best)
                              (< (first best) (first next)))
                      (setf best next))))))
         best)))))

(defun generate-field (size dirty-num)
  (let ((str (with-output-to-string (stream)
               (loop for i below size do
                    (loop for j below size do
                         (princ #\- stream))
                    (princ #\Newline stream)))))
    (setf (aref str 0) #\b)
    (loop while (< 0 dirty-num)
       do
         (let* ((i (random size))
                (j (random size))
                (coord (+ (* i size) j)))
           (when (eq (aref str coord) #\-)
             (setf (aref str coord) #\d)
             (decf dirty-num))))
    
    str))

(defun do-correlation-test (runs size dirty-num)
  ;; TODO: estimate using some statistical tools like
  ;;       std deviation?
  (let ((brute-force-total-reward 0)
        (mcts-total-reward 0))
    (loop for i below runs
       do
         (let ((field (generate-field size dirty-num)))
           (incf brute-force-total-reward
                 (nth-value 1 (brute-force-play field)))
           (incf mcts-total-reward
                 (nth-value 1 (play field)))))
    (let ((brute-force-mean (/ brute-force-total-reward runs))
          (mcts-mean (/ mcts-total-reward runs)))
      (format t "Avg. expected value    : ~,3F~%" brute-force-mean)
      (format t "Avg. actual value      : ~,3F~%" mcts-mean)
      (format t "Avg. actual / expected : ~,3F~%"
              (/ mcts-mean brute-force-mean)))))
