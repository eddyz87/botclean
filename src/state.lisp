(defpackage :src/state
  (:nicknames :state)
  (:use :common-lisp :src/field :src/utils
        :src/path)
  (:import-from :mcts)
  (:import-from :spatial-trees)
  (:import-from :rectangles))

(in-package :src/state)

(defclass base-state ()
  ((field :accessor field
          :initarg :field)
   (bot-position :accessor bot-position
                 :initarg :bot-position)
   (moves-count :accessor moves-count
                :initarg :moves-count
                :initform 0)
   (moves :accessor moves
          :initarg :moves
          :initform nil)
   ;; Dirty cells collection
   (dirty-cells :accessor dirty-cells
                :initarg :dirty-cells)
   ;; Penalty from failed tactics
   (penalty :accessor penalty
            :initform 0)))

;; Cells set

(defclass cells-set ()
  ((s-tree :initarg :s-tree
           :accessor s-tree)
   (size :initform 0
         :accessor size)
   (width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)))

(defun pos-rect (pos)
  (if (typep pos 'cells-set)
      (with-slots (width height) pos
        (rectangles:make-rectangle
         :lows (list 0 0)
         :highs (list (* width 3)
                      (* height 3))))
      (with-accessors ((x pos-x)
                       (y pos-y)) pos
        (rectangles:make-rectangle
         :lows (list (* x 3) (* y 3))
         :highs (list (+ (* x 3) 2)
                      (+ (* y 3) 2))))))

(defun pos-list (pos)
  (with-accessors ((x pos-x)
                   (y pos-y)) pos
    (list x y)))

(defun make-cells-set (width height)
  (make-instance
   'cells-set
   :s-tree
   (spatial-trees:make-spatial-tree
    :r
    :rectfun #'pos-rect)
   :width width :height height))

(defun cells-set-size (cells-set)
  (size cells-set))

(defun cells-set-to-list (cells-set)
  (spatial-trees:search cells-set (s-tree cells-set)))

(defun cells-set-get (cells-set pos)
  (car (spatial-trees:search pos (s-tree cells-set))))

(defun cells-set-add (cells-set pos)
  (spatial-trees:insert pos
                        (s-tree cells-set))
  (incf (size cells-set)))

(defun cells-set-delete (cells-set pos)
  (spatial-trees:delete pos
                        (s-tree cells-set))
  (decf (size cells-set)))

(defun dirty-cells-num (state)
  (cells-set-size (dirty-cells state)))

(defun cells-set-clone (cells-set)
  (let ((clone (make-cells-set (width cells-set)
                               (height cells-set))))
    (loop for obj in (cells-set-to-list cells-set) do
         (cells-set-add clone obj))
    clone))

(defun distance (pos-from pos-to)
  (labels ((%lst (pos)
             (if (listp pos)
                 pos
                 (list (pos-x pos)
                       (pos-y pos)))))
    (destructuring-bind (x-from y-from)
        (%lst pos-from)
      (destructuring-bind (x-to y-to)
          (%lst pos-to)
        (+ (abs (- x-from
                   x-to))
           (abs (- y-from
                   y-to)))))))

(defun cells-set-find-nearest (cells-set pos)
  (spatial-trees.nns:nearest-neighbor-search
   (pos-list pos)
   (s-tree cells-set)
   #'distance))

;;

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
         (dirty-cells (make-cells-set width height)))
    (loop for line in lines
       for y from 0 do
         (loop for ch across line
            for x from 0 do
              (case ch
                (#\b (setf bot-position
                           (make-pos :x x :y y)))
                (#\d (cells-set-add dirty-cells (make-pos :x x :y y))
                     (if mutable
                         (put-cell! field (make-pos :x x :y y) :dirt)
                         (setf field
                               (put-cell field (make-pos :x x :y y) :dirt))))
                (#\# (if mutable
                         (put-cell! field (make-pos :x x :y y) :wall)
                         (setf field
                               (put-cell field (make-pos :x x :y y) :wall)))))))
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

;;;;;;; Tactics with A*

(defgeneric advance-state-on-action (game state action))

(defmethod advance-state-on-action ((game bot-game-mutable-state) state action)
  (if (eq action :clean)
      (progn
        ;; (format t "Clean: pos ~A, cell = ~A~%"
        ;;         (bot-position state)
        ;;         (get-cell (field state) (bot-position state)))
        (when (eq (get-cell (field state) (bot-position state))
                  :dirt)
          (cells-set-delete
           (dirty-cells state)
           (cells-set-get (dirty-cells state) (bot-position state))))
        (put-cell! (field state) (bot-position state) nil))
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
  (push action (moves state))
  state)

;;;;;;; MCTS methods

(defmethod mcts:possible-actions ((game bot-game-mutable-state) state)
  (when (= (dirty-cells-num state) 0)
    (return-from mcts:possible-actions nil))
  (cells-set-to-list (dirty-cells state)))

(defmethod mcts:next-state ((game bot-game-mutable-state) state action)
  (let* ((path (a-star-find-path (field state) (bot-position state) action))
         (move-actions (path-to-moves path)))
    (if move-actions
        (progn
          (loop for move-action in move-actions do
               (advance-state-on-action game state move-action))
          (advance-state-on-action game state :clean))
        (incf (penalty state)
              100000)))
  state)

(defmethod mcts:clone-state ((game bot-game-mutable-state) state)
  (copy-instance state
                 :field (clone-field (field state))
                 :dirty-cells (cells-set-clone (dirty-cells state))))

(defparameter *num-random-estimations* 10)

;; (defmethod mcts:estimate-state-reward ((game bot-game-mutable-state) state)
;;   (labels ((%loop (state num-iters)
;;              (let ((actions (mcts:possible-actions game state)))
;;                (if (or (null actions) (= num-iters 0))
;;                    (- (/ (- 200 (moves-count state))
;;                          40)
;;                       (* 5 (dirty-cells-num state))
;;                       (penalty state))
;;                    (let ((move (random (length actions))))
;;                      (%loop (mcts:next-state game state (nth move actions))
;;                             (1- num-iters)))))))
;;     (loop for _ from 1 to *num-random-estimations* maximize
;;          (%loop (mcts:clone-state game state)
;;                 (* (width (field state))
;;                    (height (field state)))))))

(defmethod mcts:estimate-state-reward ((game bot-game-mutable-state) state)
  (labels ((%loop (state num-iters)
             (if (or (= 0 (dirty-cells-num state))
                     (= num-iters 0))
                 (- (/ (- 200 (moves-count state))
                       40)
                    (* 5 (dirty-cells-num state))
                    (penalty state))
                 (let ((nearest-pos (cells-set-find-nearest
                                     (dirty-cells state)
                                     (bot-position state))))
                   (%loop (mcts:next-state game state nearest-pos)
                          (1- num-iters))))))
    (%loop (mcts:clone-state game state)
           (* (width (field state))
              (height (field state))))))

;;;;;;;;;;;;;;;

(defun visualize-state (state stream)
  (format stream "Moves: ~A~%Dirty cells: ~A~%"
          (moves-count state)
          (dirty-cells-num state))
  (loop for y from 0 to (1- (height (field state))) do
       (loop for x from 0 to (1- (width (field state))) do
            (let* ((pos (make-pos :x x :y y ))
                   (val (get-cell (field state) pos)))
              (princ (if (equalp pos (bot-position state))
                         #\b
                         (case val
                           (:dirt #\d)
                           (:wall #\#)
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

(defun sample-field-3 ()
  (format nil "~{~A~%~}"
          '("---#d"
            "---#d"
            "-b-#d"
            "---#d"
            "-dd#-"
            "-----")
          ))

(defun sample-field-4 ()
  (format nil "~{~A~%~}"
          '("-#--d-----------------------"
            "-#d--------------------d----"
            "-#bd--------------------dd--"
            "-#########################--"
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
                       (= (dirty-cells-num state) 0))
                   state
                   (let* ((action (mcts:select-next-move game state 0.5))
                          (new-state (mcts:next-state game state action)))
                     (%step new-state (1- num-moves))))))
      (%step root-state max-moves))))

(defun mutable-mcts-play (field-str &key (visualize t) (max-moves 100))
  (mcts-play field-str 'array-field 'mutable-state 'bot-game-mutable-state
             :visualize visualize
             :max-moves max-moves))
