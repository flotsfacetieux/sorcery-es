(in-package :sorcery-es)

(defclass player-system (system) ())

(defun update-bag-costume (entity-manager)
  (when-let* ((bag (bag-entity))
	      (costume (entity-component entity-manager
					 bag 'costume)))
    (setf (costume-data costume)
	  (if-let ((item (player-bag-item entity-manager)))
	    (when-let* ((item-type (item-type item))
			(cell (item-atlas-cell item-type)))
	      (list (list :front (list cell))))
	    '()))))

(defmethod update ((system player-system) entity-manager)
  (update-bag-costume entity-manager)
  (player-default-move entity-manager)
  (use-item entity-manager))
