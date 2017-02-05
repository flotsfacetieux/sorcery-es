(in-package :sorcery-es)

(defclass roll-text-system (system)
  ((dt-accumulator :accessor dt-accumulator
		   :initform 0)
   (dt-max :accessor dt-max
	   :initform 40)))

(defmethod update ((system roll-text-system) entity-manager)
  (if (>= (dt-accumulator system) (dt-max system))
      (dolist (e-text (find-entities entity-manager 'roll-text))
	(let ((roll-text (entity-component entity-manager
					   e-text 'roll-text))
	      (text (entity-component entity-manager
				      e-text 'text)))
	  
	  (let ((txt-width (text-pixel-width text)))
	    (if (>= (+ (text-x text) txt-width) (x-left roll-text))
		(decf (text-x text) (rlt-step roll-text))
		(setf (text-x text) (x-right roll-text)))
	    (setf (dt-accumulator system) 0))))
      (incf (dt-accumulator system) (sdl:dt))))
