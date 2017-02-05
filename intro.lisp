(in-package :sorcery-es)

(defun init-intro (state)
  (let ((entity-manager (make-instance 'entity-manager)))
    (cheat-code-reset)
    (make-game-entity (entity-manager)
     (make-instance 'text
		    :string "Press space to start game"
		    :x *canvas-x*
		    :y (+ *origin-y* 360))
     (make-instance 'roll-text
		    :step 2
		    :x-left 0
		    :x-right *canvas-x*))
    
    (make-fire entity-manager :x 6 :y 38)
    (make-fire entity-manager :x 292 :y 72)
    (make-fire entity-manager :x 570 :y 38)
    (make-game-entity
     (entity-manager) 
     (make-instance 'background
		    :atlas-cell 70
		    :sheet *areas-sheet*))
    (setf (gethash :intro *entities*) entity-manager)
    (setf (state-em state) entity-manager)
    (switch-state :intro)))

(defun intro-state ()
  (make-instance
   'state
   :data (list :system (make-instance 'roll-text-system))
   :engine #'(lambda (state)
	       (let ((em (state-em state)))
		 (update (getf (state-data state) :system) em)))
   :keyboard
   (list :down
	 (list
	  (cons :sdl-key-q #'(lambda ()
			       (sdl:push-quit-event)))
	  (cons :sdl-key-a #'(lambda ()
			       (cheat-code-key #\a)))
	  (cons :sdl-key-u #'(lambda ()
			       (cheat-code-key #\u)))
	  (cons :sdl-key-r #'(lambda ()
			       (cheat-code-key #\r)))
	  (cons :sdl-key-m #'(lambda ()
			       (cheat-code-key #\m)))
	  (cons :sdl-key-space #'(lambda ()
				   (let ((state (dict-state :game)))
				     (init-game state)
				     (switch-state :game))))))))

 
