(in-package :sorcery-es)

(defclass victory-system (system)
  ((end-count :accessor system-end-count
	      :initform 0)
   (dt-accumulator :accessor dt-accumulator
		   :initform 0)
   (dt-max :accessor dt-max
	   :initform 3600)))

(defmethod update ((system victory-system) entity-manager)
  (let ((release (entity-component entity-manager
				     (player-entity)
				     'release)))
    (when (= (system-end-count system) (release-amount release))
      (let ((state (dict-state :game)))
	(setf (getf (state-data state) :game-status) :end-game)
	(make-game-entity (entity-manager)
	  (make-instance 'countdown
			 :index 20
			 :step-action ()
			 :end-action #'swap-to-summary)))))
  (if (>= (dt-accumulator system) (dt-max system))
      (block victory
	(dolist (costume (find-components entity-manager 'costume))
	  (when  (and (assoc :victory (costume-data costume))
		      (not (equal :victory
				  (costume-direction costume))))
	    (setf (costume-index costume) 0)
	    (setf (costume-direction costume) :victory)
	    (incf (system-end-count system))
	    (return-from victory))
	  (setf (dt-accumulator system) 0)))
      (incf (dt-accumulator system) (sdl:dt))))
