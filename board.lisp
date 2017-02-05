(in-package :sorcery-es)

;;

(defun board-entity-id (state key)
  (getf (state-data-elmt state key) :id))

(defun board-entity-components (state key)
  (getf (state-data-elmt state key) :components))

(defun (setf board-entity-id) (id state key)
  (let ((board (state-data-elmt state key)))
    (setf (getf board :id) id)
    (setf (state-data-elmt state key) board)))

(defun (setf board-entity-components) (components state key)
  (let ((board (state-data-elmt state key)))
    (setf (getf board :components) components)
    (setf (state-data-elmt state key) board)))
