(in-package :sorcery-es)

;; System for managing objects coming out of the game board

(defclass outside-system (system) ())

(defmethod update ((system outside-system) entity-manager)
  (dolist (entity (find-entities entity-manager 'move))
    (let ((move (entity-component entity-manager entity 'move))
	  (bb (entity-component entity-manager entity 'bounding-box)))
      (when (and (move-outsidep move) (not (inside-boundaries-p bb)))
	(remove-entity entity-manager entity)))))
