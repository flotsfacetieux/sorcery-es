(in-package :sorcery-es)

(defclass costume-system (system)
  ((dt-accumulator :accessor dt-accumulator
		   :initform 0)
   (dt-max :accessor dt-max
	   :initform 800)))

;; Same dt for all costumes ...
;; A better choice will be a different dt

(defun update-costume-direction (entity-manager entity &optional direction)
  (let ((costume (entity-component entity-manager entity 'costume)))
    (when-let
	((cdata (costume-data costume))
	 (entity-move (entity-component entity-manager
					entity
					'move)))
      (let* ((stack (move-stack entity-move))
	     (direction* (or direction
			     (and stack (find :right stack))
			     (and stack (find :left stack))
			     :front)))
	(when (and
	       (not (equal direction* (costume-direction costume)))
	       (assoc direction* cdata))
	  (setf (costume-index costume) 0)
	  (setf (costume-direction costume) direction*))))))

(defun rotate-costume-index (costume)
  (let ((sprites-count (costume-sprites-count costume)))
    (incf (costume-index costume))
    (when (>= (costume-index costume) sprites-count)
      (setf (costume-index costume) 0))))

(defun rotate-entity-costume (entity-manager entity)
  (let ((animation (entity-component entity-manager
				     entity
				     'animation))
	(costume (entity-component entity-manager entity 'costume)))
    (if (and animation
	     (= (costume-index costume)
		(1- (costume-sprites-count costume))))
	(funcall (animation-action animation) entity-manager entity)
	(rotate-costume-index costume))))

(defmethod update ((system costume-system) entity-manager)
  (if (>= (dt-accumulator system) (dt-max system))
      (dolist (entity (find-entities entity-manager 'costume))
	(rotate-entity-costume entity-manager entity)
	(setf (dt-accumulator system) 0))
      (incf (dt-accumulator system) (sdl:dt))))


