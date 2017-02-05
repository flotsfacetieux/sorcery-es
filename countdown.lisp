(in-package :sorcery-es)

(defun deface-time-book (entity-manager entity)
  ;; draws a black pixel at each step of the countdown
  (let* ((countdown (entity-component entity-manager
				      entity 'countdown))
	 (costume (entity-component entity-manager
				    entity 'costume))
	 (surface (sheet (costume-sheet costume)))
	 (w (sdl:width surface))
	 (h (sdl:height surface))
	 (index (- (* w h) (countdown-index countdown))))
    (sdl:draw-pixel-* (mod index w)
		      (truncate index w)
		      :surface surface :color sdl:*black*)))

(defclass countdown-system (system)
  ((dt-accumulator :accessor dt-accumulator
		   :initform 0)
   (dt-max :accessor dt-max
	   :initform 500)))

(defmethod update ((system countdown-system) entity-manager)
  (if (>= (dt-accumulator system) (dt-max system))
      (dolist (entity (find-entities entity-manager 'countdown))
	(let ((countdown (entity-component entity-manager
					   entity 'countdown)))
	  (if (<= (countdown-index countdown) 0)
	      (funcall (countdown-end-action countdown)
		       entity-manager entity)
	      (let ((action (countdown-step-action countdown)))
		(when action
		  (funcall action entity-manager entity))
		(decf (countdown-index countdown))
		(setf (dt-accumulator system) 0)))))
      (incf (dt-accumulator system) (sdl:dt))))
