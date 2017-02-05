(in-package :sorcery-es)

(defclass contact-system (system)
  ())

(defun open-door-p (entity-manager door)
  (let ((lock (entity-component entity-manager door 'lock))
	(item (player-bag-item entity-manager)))
    (or *cheat-mode*
	(not (lockedp lock))
	(and item (unlockp lock (item-type item))))))

(defun contact-door-right (entity-manager entity-bounding-box door-bounding-box)
  (when-let ((move (entity-component entity-manager
				 (player-entity) 'move)))
    (and (find :right (move-stack move))
     (<= (abs (- (y entity-bounding-box) (y door-bounding-box))) 2)
     (<= (abs (- (x entity-bounding-box) (x door-bounding-box))) (width entity-bounding-box)))))

(defun contact-door-left (entity-manager entity-bounding-box door-bounding-box)
  (when-let ((move (entity-component entity-manager
				(player-entity) 'move)))
    (and (find :left (move-stack move))
     (<= (abs (- (y entity-bounding-box) (y door-bounding-box))) 2)
     (<= (abs (- (x entity-bounding-box) (x door-bounding-box))) (width entity-bounding-box)))))

(defun contact-door-green (entity-manager entity-bounding-box door-bounding-box)
  (declare (ignore entity-manager))
  (and
   (<= (abs (- (y entity-bounding-box) (y door-bounding-box))) 2)
   (<= (abs (- (x entity-bounding-box) (x door-bounding-box))) 32)))

(defun contact-door-trap (entity-manager entity-bounding-box door-bounding-box)
  (declare (ignore entity-manager))
  (and
   (<= (abs (- (y entity-bounding-box) (y door-bounding-box))) 16)
   (<= (abs (- (x entity-bounding-box) (x door-bounding-box))) 2)))

(defun contact-pedestal (entity-manager entity-bounding-box pedestal-bounding-box)
  (declare (ignore entity-manager))
  (and
   (<= (abs (- (y entity-bounding-box) (y pedestal-bounding-box))) 48)
   (<= (abs (- (x entity-bounding-box) (x pedestal-bounding-box))) 2)))

(defun contact-cauldron (entity-manager entity-bounding-box cauldron-bounding-box)
  (and
   (<= (abs (- (y entity-bounding-box) (y cauldron-bounding-box))) 30)
   (<= (abs (- (x entity-bounding-box) (x cauldron-bounding-box))) 12)))

(defun contact-player-open-object (entity-manager entity)
  (when (open-door-p entity-manager entity)
    (setf *swap-door* entity)))

(defun contact-player-remove-object (entity-manager entity)
  (when (open-door-p entity-manager entity)
    (when (player-entity)
       (let ((score (entity-component entity-manager (player-entity) 'score)))
	 (incf (score-value score) 100)))
    (empty-player-bag entity-manager)
    (remove-entity entity-manager entity)))

(defmethod update ((system contact-system) entity-manager)
  (when (player-entity)
    (let ((player-bb (entity-component entity-manager
				       (player-entity)
				       'bounding-box)))

      (dolist (entity (find-entities entity-manager 'contact))
	(let ((e-contact (entity-component entity-manager
					   entity
					   'contact))
	      (e-bb (entity-component entity-manager
				      entity
				      'bounding-box)))
	  (when (funcall (contact-test e-contact)
			 entity-manager player-bb e-bb)
	    (funcall (contact-action e-contact)
		     entity-manager entity)))))))
