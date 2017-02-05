(in-package :sorcery-es)

(defun init-game (state)
  ;; Game initialization
  (let* ((start (start-area))
	 (area (car start))
	 (pos (cadr start)))

    (init-areas)
    (init-player state pos)
    (init-released-friends-positions)

    (setf *game-start-time* (get-universal-time))

    (setf *swap-door* nil)
    (setf *swap-area-switch* nil)
    (setf *player-keyboard-stack* ())
    (setf *pause-flag* nil)
    (setf (getf (state-data state) :game-status) :play)
    (setf (state-em state) (area-entities area))
    (player-swap-area :to area)))

(defun game-status (state)
  (getf (state-data state) :game-status))

(defun (setf game-status) (status state)
  (setf (getf (state-data state) :game-status) status))

(defun game-state ()
  (let ((cds (make-instance 'countdown-system)))
   (make-instance
    'state
    :data (list :core (list (make-instance 'player-system)
			    (make-instance 'outside-system)
			    (make-instance 'move-system)
			    (make-instance 'collide-system)
			    (make-instance 'contact-system)
			    cds
			    (make-instance 'text-system))
		:victory (list cds
			       (make-instance 'victory-system))
		:end-game cds)
    :engine #'(lambda (state)
		(let ((entity-manager (state-em state))
		      (data (state-data state)))
		 
		 
		  (case (getf data :game-status)
		    (:play (if *swap-door*
			       (open-door entity-manager *swap-door*)
			       (dolist (system (getf data :core))
				 (update system entity-manager))))
		    (:swap-door
		     (when *swap-area-switch*
		       (let* ((door (entity-component
				     entity-manager
				     *swap-door*
				     'door))
			      (next-area (door-target door))
			      (next-door (next-area-door
					  entity-manager
					  *swap-door*)))
			
			 (player-swap-area :from entity-manager
					   :to next-area)
			 (set-player-position-to
			  (area-entities next-area) next-door)
			 (close-door (area-entities next-area)
				     next-door)
			 (reset-player-move (area-entities next-area))
			 (update-costume-direction
			  (area-entities next-area)
			  (player-entity)
			  :front)
			 (setf *swap-door* nil)
			 (setf *swap-area-switch* nil))))
		    (:victory (dolist (system (getf data :victory))
				(update system entity-manager)))
		    (:end-game (update (getf data :end-game)
				       entity-manager))
		    )))

    :keyboard
    (list :up
	  (list
	   (cons :sdl-key-space
		 #'(lambda ()
		     (switch-off-player-fire)))
	   (cons :sdl-key-right
		 #'(lambda ()
		     (setf *player-keyboard-stack*
			   (remove :right *player-keyboard-stack*))))
	   (cons :sdl-key-left
		 #'(lambda ()
		     (setf *player-keyboard-stack*
			   (remove :left *player-keyboard-stack*))))
	   (cons :sdl-key-p
		 #'(lambda ()
		     (setf *player-keyboard-stack*
			   (remove :right *player-keyboard-stack*))))
	   (cons :sdl-key-o
		 #'(lambda ()
		     (setf *player-keyboard-stack*
			   (remove :left *player-keyboard-stack*))))
	   (cons :sdl-key-a
		 #'(lambda ()
		     (setf *player-keyboard-stack*
			   (remove :top *player-keyboard-stack*))))
	   (cons :sdl-key-up
		 #'(lambda ()
		     (setf *player-keyboard-stack*
			   (remove :top *player-keyboard-stack*)))))
	 
	  :down
	  (list
	   (cons :sdl-key-q #'(lambda ()
				(sdl:push-quit-event)))
	   (cons :sdl-key-space #'(lambda ()
				    (switch-on-player-fire)))
	   (cons :sdl-key-right #'(lambda ()
				    (pushnew :right
					     *player-keyboard-stack*)))
	   (cons :sdl-key-left #'(lambda ()
				   (pushnew :left *player-keyboard-stack*)))
	   (cons :sdl-key-p #'(lambda ()
				
				(pushnew :right *player-keyboard-stack*)))
	   (cons :sdl-key-o #'(lambda ()
				
				(pushnew :left *player-keyboard-stack*)))
	   (cons :sdl-key-up #'(lambda ()
				 
				 (pushnew :top *player-keyboard-stack*)))
	   (cons :sdl-key-a #'(lambda ()
				
				(pushnew :top *player-keyboard-stack*)))
	   (cons :sdl-key-n #'(lambda ()
				(let ((next-area
				       (next-area-id *current-area*)))
				  (player-swap-area :to next-area
						    :from *current-area*))))
	   (cons :sdl-key-r #'(lambda ()
				(let ((state (dict-state :game)))
				  (init-game state)
				  (cheat-code-reset)
				  (switch-state :intro)
				  (setf *current-area* :intro))))
	   (cons :sdl-key-x #'(lambda ()
				(let ((state (dict-state :game)))
				  (setf (game-status state)
					(if (equal (game-status
						    state)
						   :pause)
					    :play
					    :pause))))))))))
