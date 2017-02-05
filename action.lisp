(in-package :sorcery-es)


(defun reset-door (entity-manager door-entity)
  ;; Reset door :
  ;; - modify costume
  ;; - delete "animation" component
  (let ((costume (entity-component entity-manager door-entity 'costume)))
    (setf (costume-direction costume) :front)
    (setf (costume-index costume) 0)
    (del-components entity-manager door-entity 'animation)))

(defun open-door (entity-manager door-entity)
  ;; start "open-door" animation
  ;; - add "animation" component
  (let ((costume (entity-component entity-manager
				   door-entity 'costume))
	(state (dict-state :game)))
    (setf (costume-direction costume) :open)
    (setf (costume-index costume) 0)
    (add-component entity-manager door-entity
		   (make-instance 'animation
				  :action
				  #'(lambda (entity-manager entity)
				      (reset-door entity-manager
						  entity)
				      (setf *swap-area-switch* t))))

    (setf (game-status state) :swap-door)))

(defun close-door (entity-manager door-entity)
  ;; start "close-door" animation
  ;; - add "animation" component
  (let ((costume (entity-component entity-manager
				   door-entity 'costume))
	(state (dict-state :game)))
    (setf (costume-direction costume) :close)
    (add-component entity-manager door-entity
		   (make-instance 'animation
				  :action
				  #'(lambda (entity-manager entity)
				      (reset-door entity-manager
						  entity)
				      (setf (game-status state) :play))))))

(defun swap-to-summary (entity-manager entity)
  (declare (ignore entity))
  (let ((score (entity-component entity-manager
				 (player-entity) 'score))
	(release (entity-component entity-manager
				   (player-entity)
				   'release)))
    (init-summary (dict-state :summary)
		  (score-value score)
		  (release-amount release)
		  (- (get-universal-time) *game-start-time*))
      (switch-state :summary)))

(defun victory (entity-manager entity)
  (declare (ignore entity))
  (when (player-entity)
    (let ((release (entity-component entity-manager
				     (player-entity)
				     'release))
	  (score (entity-component entity-manager
				   (player-entity)
				   'score)))
      (when (= (release-amount release) (release-max release))
	(add-component
	 entity-manager
	 (player-entity)
	 (make-instance 'text
			:x (+ *origin-x* 280)
			:y (+ *origin-y* 20)
			:string "!! VICTORY !!"))
	(incf (score-value score) 10000)
	(setf (game-status *current-state*) :victory)))))

(defun add-friend-M6 ()
  (let ((position (pop *released-friends-positions*)))
    (make-friend (gethash :M6 *entities*)
		 :x (car position)
		 :y (cdr position)
		 :status nil)))

(defun release-friend (entity-manager entity)
   (let ((entity-bounding-box (entity-component entity-manager entity 'bounding-box)))
     (make-release-anim entity-manager
		     :x (x entity-bounding-box)
		     :y (y entity-bounding-box))
     (when (player-entity)
       (let ((release (entity-component entity-manager (player-entity) 'release))
	     (score (entity-component entity-manager (player-entity) 'score)))
	 (incf (release-amount release))
	 (incf (score-value score) 1000)))
     (add-friend-M6)
     (remove-entity entity-manager entity)))

(defun reset-player-move (entity-manager)
  ;; reset player move stack.
  ;; used when player cross a door.
  (setf *player-keyboard-stack* ())
  (reset-entity-move entity-manager (player-entity)))

(defun player-swap-area (&key to from)
  (when-let ((entity-manager (area-entities to))
	     (state (dict-state :game)))
    (let ((board-key-entities '(:player :timebook :bag)))
      (setf (state-em state) entity-manager)
      (dolist (entity-key board-key-entities)
	(when from
	  (remove-entity from (board-entity-id state entity-key)))
	(let ((entity (make-entity entity-manager))
	      (components (board-entity-components state entity-key)))
	  (setf (board-entity-id state entity-key) entity)
	  (dolist (component components)
	    (when component
	      (add-component entity-manager entity component))))))))

(defun find-door (entity-manager door-id)
  (loop for entity in (find-entities entity-manager 'door)
     when (equal door-id (door-id (entity-component entity-manager entity 'door)))
     do (return-from find-door entity)))

(defun entrance (door-opening x-pos)
  (case door-opening
    (:right (- x-pos (sprite-width) 0))
    (:left (+ x-pos (sprite-width) 0))))

(defun next-area-door (entity-manager door-entity)
  ;; return the entity of the door of the next area
  (let* ((door (entity-component entity-manager door-entity 'door))
	 (em (area-entities (door-target door))))
    (find-door em (door-id door))))

(defun reset-entity-move (entity-manager entity)
  (let ((move (entity-component entity-manager entity 'move)))
    (reset-move-stack move)))

(defun set-player-position-to (entity-manager door-entity)
  ;; set player position near the door
  (let ((door (entity-component entity-manager door-entity 'door))
	(door-bb (entity-component entity-manager
				   door-entity
				   'bounding-box))
	(player-bb (entity-component entity-manager
				     (player-entity)
				     'bounding-box)))
    (setf (x player-bb)
	    (entrance (door-opening door) (x door-bb)))
    (setf (y player-bb)
	  (y door-bb))))

(defun switch-on-player-fire ()
  (setf *player-fire-switch* t))

(defun switch-off-player-fire ()
  (setf *player-fire-switch* nil))

(defun use-item (entity-manager)
  ;; Manage player fire action
  (when (and (player-entity) *player-fire-switch*)
    (when-let ((item (player-bag-item entity-manager))
	       (player-bb (entity-component entity-manager
					    (player-entity)
					    'bounding-box)))
      
      (when-let ((action (item-action item)))
	(funcall action
		 entity-manager
		 :x (x player-bb)
		 :y (y player-bb))
	(empty-player-bag entity-manager)
	(switch-off-player-fire)))))

(defun kill-monster (entity-manager entity)
  (let ((bb (entity-component entity-manager entity 'bounding-box)))
    (make-explosion entity-manager :x (x bb) :y (y bb))
    (when (player-entity)
      (let ((score (entity-component entity-manager (player-entity) 'score)))
	(incf (score-value score) 500)))
    (remove-entity entity-manager entity)))

(defun player-die (entity-manager)
  (let ((state (dict-state :game)))
    (del-components entity-manager
		    (player-entity)
		    'move)
    (del-components entity-manager
		    (player-entity)
		    'costume)
    (del-components entity-manager
		    (player-entity)
		    'stencil)
    (make-game-entity (entity-manager)
      (make-instance 'countdown
		     :index 20
		     :step-action ()
		     :end-action #'swap-to-summary))
    (setf (game-status state) :end-game)))

(defun player-drown (entity-manager entity)
  (declare (ignore entity))
  (when (and (player-entity) (not *cheat-mode*))
    (let ((player-bb (entity-component entity-manager (player-entity) 'bounding-box)))
      (make-drown entity-manager
  		  :x (x player-bb)
  		  :y (y player-bb))
      (player-die entity-manager))))

(defun player-explose (entity-manager entity)
  (declare (ignore entity))
  (let ((player-bb (entity-component entity-manager
				     (player-entity)
				     'bounding-box)))
    (make-explosion entity-manager
		    :x (x player-bb)
		    :y (y player-bb))
    (player-die entity-manager)))

(defun restore-energy (entity-manager entity)
  (declare (ignore entity))
  (when (player-entity)
    (let ((energy (entity-component entity-manager (player-entity) 'energy)))
      (when (and energy (< (energy-amount energy) 99))
	(if (>= (energy-dt-acc energy) (energy-dt-inc energy))
	    (progn  (increase-energy energy)(setf (energy-dt-acc energy) 0))
	    (incf (energy-dt-acc energy) (sdl:dt)))))))

(defun drain-energy (entity-manager entity)
  (declare (ignore entity))
  (when (and (player-entity) (not *cheat-mode*))
    (let ((energy (entity-component entity-manager (player-entity) 'energy)))
      (when energy
	(if (> (energy-amount energy) 0)
	    (if (>= (energy-dt-acc energy) (energy-dt-dec energy))
		(progn
		  (decrease-energy energy)
		  (setf (energy-dt-acc energy) 0))
		(incf (energy-dt-acc energy) (sdl:dt)))
	    (player-explose entity-manager (player-entity)))))))

(defun player-bag-item (entity-manager)
  (when (player-entity)
    (let ((bag (entity-component entity-manager (player-entity) 'bag)))
      (when bag
	(bag-item bag)))))

(defun empty-player-bag (entity-manager)
  (when (player-entity)
    (let ((bag (entity-component entity-manager (player-entity) 'bag)))
      (when bag
	(setf (bag-item bag) nil)))))

(defun player-use-right-item-p (entity-manager monster-entity)
  (let ((monster-vulnerability
	 (entity-component entity-manager
			   monster-entity
			   'vulnerability))
	(item (player-bag-item entity-manager)))
    (and *player-fire-switch*
	 item
	 (find (item-type item) (weapons monster-vulnerability)))))

(defun bag-store-item (entity-manager item-entity)
  (when (player-entity)
    (let ((bag (entity-component entity-manager (player-entity) 'bag))
	  (item (entity-component entity-manager item-entity 'item)))
      (setf (bag-item bag) item)
      (remove-entity entity-manager item-entity))))

(defun new-item (entity-manager item-builder x y)
  (funcall item-builder entity-manager :x x :y y))

(defun switch-bag-item (entity-manager item-entity bag-item)
  (let ((item-bb (entity-component entity-manager
				   item-entity
				   'bounding-box)))
    (bag-store-item entity-manager item-entity)

    (new-item entity-manager
	      (item-builder bag-item)
	      (x item-bb)
	      (y item-bb))))

(defun pickup-item (entity-manager item-entity)
  (when (and (player-entity) *player-fire-switch*)
    (switch-off-player-fire)
    (let ((bag-item (player-bag-item entity-manager)))
      (if bag-item
	  (switch-bag-item entity-manager item-entity bag-item)
	  (bag-store-item entity-manager item-entity)))))
