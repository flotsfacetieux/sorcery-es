(in-package :sorcery-es)

(defparameter *states-dict* ())
(defvar *current-state*)

(defun dict-state (key)
  (getf *states-dict* key))

(defun (setf dict-state) (state key)
  (setf (getf *states-dict* key) state))

;; a state game

(let (rs cs)

 (defclass state ()
   ((entity-manager :accessor state-em
		    :initarg :entity-manager)
    (data :accessor state-data
	  :initarg :data)
    (engine :accessor state-engine
	    :initarg :engine)
    (keyboard :accessor state-keyboard
	      :initarg :keyboard)))

 (defun (setf game-render-system) (render-system)
   (setf rs render-system))
 
 (defun (setf game-costume-system) (costume-system)
   (setf cs costume-system))

 (defmethod run-state ((state state))
   (let ((em (state-em state)))
     (funcall (state-engine state) state)
     (update cs em)
     (update rs em)))

 (defmethod key-event ((state state) status key)
   (when-let* ((skb (state-keyboard  state))
	       (key-dict (getf skb status))
	       (key-action (assoc key key-dict :test #'sdl:key=)))

     (funcall (cdr key-action))))

 (defun switch-state (state-key)
   (setf *current-state* (dict-state state-key))))

(defun state-data-elmt (state key)
  (getf (state-data state) key))

(defun (setf state-data-elmt) (elmt state key)
  (setf (getf (state-data state) key) elmt))

