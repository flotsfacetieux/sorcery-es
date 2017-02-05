(in-package :sorcery-es)

(defclass move-system (system) ())

(defun move-allowedp (entity-manager entity next-position )
  (let ((move (entity-component entity-manager entity 'move)))
    (and
     (or (move-outsidep move) (inside-boundaries-p next-position))
     (or (move-acrossp move)
	 (not (overlapsp entity-manager entity next-position))))))

(defun copy-bounding-box (bounding-box)
  (make-instance 'bounding-box
		 :x (x bounding-box)
		 :y (y bounding-box)
		 :width (width bounding-box)
		 :height (height bounding-box)))

(defun update-position (entity-manager entity bounding-box update-fun)
  (let ((bb (copy-bounding-box bounding-box)))
    (funcall update-fun bb)
    (if (move-allowedp entity-manager entity bb)
	bb
	bounding-box)))
    

(defun update-position-left (entity-manager entity position)
  (update-position entity-manager entity position
		   (lambda (pos) (decf (x pos) 1))))

(defun update-position-right (entity-manager entity position)
  (update-position entity-manager entity position
		   (lambda (pos)
		     (incf (x pos) 1))))

(defun update-position-top (entity-manager entity position)
  (update-position entity-manager entity position
		   (lambda (pos)
		     (decf (y pos) 1))))

(defun update-position-bottom (entity-manager entity position)
  (update-position entity-manager entity position
		   (lambda (pos)
		     (incf (y pos) 1))))

(defun next-position (entity-manager entity)
  ;; Return the next position of the entity
  (let ((entity-move (entity-component entity-manager entity 'move))
	(entity-bb (entity-component entity-manager entity 'bounding-box)))
    (when (and entity-move entity-bb)
      (let ((next-pos (copy-bounding-box entity-bb)))
	(dolist (direction (move-stack entity-move))
	  (case direction
	    (:left (setf next-pos (update-position-left entity-manager entity next-pos)))
	    (:right (setf next-pos (update-position-right entity-manager entity next-pos)))
	    (:top (setf next-pos (update-position-top entity-manager entity next-pos)))
	    (:bottom (setf next-pos (update-position-bottom entity-manager entity next-pos)))))
	next-pos))))

(defun move-to (entity-manager entity direction)
  ;; add direction to entity move stack 
  (when-let
      ((entity-move (entity-component entity-manager entity 'move)))
    (pushnew direction (move-stack entity-move))))

(defun move-left (entity-manager entity)
  (move-to entity-manager entity :left))

(defun move-right (entity-manager entity)
  (move-to entity-manager entity :right))

(defun move-top (entity-manager entity)
  (move-to entity-manager entity :top))

(defun move-bottom (entity-manager entity)
  (move-to entity-manager entity :bottom))

(defun move-to- (entity-manager entity direction)
  ;; delete direction from entity move stack 
  (when-let*
      ((entity-move (entity-component entity-manager entity 'move)))
    (remove-direction entity-move direction)))

(defun move-left- (entity-manager entity)
  (move-to- entity-manager entity :left))

(defun move-right- (entity-manager entity)
  (move-to- entity-manager entity :right))

(defun move-top- (entity-manager entity)
  (move-to- entity-manager entity :top))

(defun move-bottom- (entity-manager entity)
  (move-to- entity-manager entity :bottom))

(defun hunt-down (entity-manager entity)
  (when (player-entity)
    (let* ((player-bb (entity-component entity-manager
					(player-entity)
					'bounding-box))
	   (monster-bb (entity-component entity-manager
					 entity
					 'bounding-box))
	   (x-step (signum (- (x player-bb)
			      (x monster-bb))))
	   (y-step (signum (- (y player-bb)
			      (y monster-bb)))))
      (case x-step
	(-1 (move-left entity-manager entity))
	(1 (move-right entity-manager entity)))
      (case y-step
	(-1 (move-top entity-manager entity))
	(1 (move-bottom entity-manager entity))))))

(defun hunt-down-h (entity-manager entity)
  (when (player-entity)
    (let* ((player-bounding-box
	    (entity-component entity-manager
			      (player-entity)
			      'bounding-box))
	   (monster-bounding-box (entity-component entity-manager
						   entity
						   'bounding-box))
	   (x-step (signum (- (x player-bounding-box)
			      (x monster-bounding-box)))))
      (case x-step
	(-1 (move-left entity-manager entity))
	(1 (move-right entity-manager entity))))))

(defun player-default-move (entity-manager)
  ;; Default player move
  ;; if the keyboard "up" action is not used, the player entity falls
  (declare (ignore entity-manager))
  (when (player-entity)
    (if (find :top *player-keyboard-stack*)
	(setf *player-keyboard-stack* (remove :bottom *player-keyboard-stack*))
	(pushnew :bottom *player-keyboard-stack*))))

(defun player-keyboard-move (entity-manager entity)
  (when (player-entity)
    (when-let ((player-move (entity-component
			     entity-manager
			     (player-entity) 'move)))
      (setf (move-stack player-move) *player-keyboard-stack*))))

(defun allow-move (bounding-box next-bounding-box)
  (setf (x bounding-box) (x next-bounding-box))
  (setf (y bounding-box) (y next-bounding-box)))

(defmethod update ((system move-system) entity-manager)
  ;; next position (may be different or not)
  (dolist (entity (find-entities entity-manager 'move))
    (let ((move (entity-component entity-manager
				  entity
				  'move))
	  (bounding-box (entity-component entity-manager
					  entity
					  'bounding-box)))

      (if (>= (move-dt-acc move) (move-speed move))
      	  (progn
	    (reset-entity-move entity-manager entity)

	    (when-let ((action (move-action move)))
	      (funcall action entity-manager entity))
	    
	    (when-let ((next-pos
			(next-position entity-manager entity)))
	      (allow-move bounding-box next-pos))
	    (update-costume-direction entity-manager entity)
	    (setf (move-dt-acc move) 0))
	  (incf (move-dt-acc move) (sdl:dt))) )))
