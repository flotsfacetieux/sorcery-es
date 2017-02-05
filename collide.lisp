(in-package :sorcery-es)

(defclass collide-system (system) ())

(defun fireballs (entity-manager)
  (let ((fireballs '())
	(items-entities (find-entities entity-manager 'item)))
    (dolist (entity items-entities)
      (let ((item (entity-component entity-manager entity 'item)))
	(when (equal 'fireball (item-type item))
	  (push entity fireballs))))
    fireballs))

(defun collide-fireball-monster (entity-manager)
  ;; if a fireball overlaps a monster, kill monster
  (dolist (fb (fireballs entity-manager))
    (dolist (monster (find-entities entity-manager 'vulnerability))
      (let ((fb-bb (entity-component entity-manager fb 'bounding-box))
	    (monster-bb (entity-component entity-manager monster 'bounding-box)))
	(when (intersectp fb-bb monster-bb)
	  (when (overlaps-pixel-p entity-manager
				  fb fb-bb
				  monster monster-bb)
	    (kill-monster entity-manager monster)))))))

(defun collide-player-monster (entity-manager monster-entity)
  ;; if the player overlaps a monster :
  ;; - if the player use the right item : kill monster
  ;; - otherwise monster drain player energy
  (when (player-entity)
    (if (player-use-right-item-p entity-manager monster-entity)
	(progn
	  (switch-off-player-fire)
	  (kill-monster entity-manager monster-entity)
	  (empty-player-bag entity-manager))
	(drain-energy entity-manager (player-entity)))))

(defun collide-player-something (entity-manager)
  ;; if the player overlaps somthing
  ;; do associated action
  (when (player-entity)
    (when-let ((player-bb (entity-component entity-manager (player-entity) 'bounding-box)))
      (dolist (entity (find-entities entity-manager 'overlaps-with-player))
	(let ((entity-bb (entity-component entity-manager entity 'bounding-box)))
	  (when (intersectp player-bb entity-bb)
	    (let ((entity-overlaps (entity-component
				    entity-manager
				    entity
				    'overlaps-with-player)))
	      (when (funcall (overlaps-test entity-overlaps)
			     entity-manager
			     (player-entity)
			     player-bb
			     entity
			     entity-bb)
		(funcall (overlaps-action entity-overlaps)
			 entity-manager
			 entity)))))))))

(defmethod update ((system collide-system) entity-manager)
  (collide-fireball-monster entity-manager)
  (collide-player-something entity-manager))
