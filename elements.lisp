(in-package :sorcery-es)

(defmacro make-game-entity ((entity-manager) &body components)
  `(let ((entity (make-entity ,entity-manager)))
     (dolist (component (list ,@components))
       (when component
	 (add-component ,entity-manager entity component)))
     entity))

(defun store-area (id area)
  (setf (gethash id *entities* nil) area))

(defmacro make-area ((area id &key name background stencil foreground)
		     &body body)
  (declare (ignore area))
  `(let ((area (make-instance 'entity-manager)))
     (make-game-entity (area)
       (make-instance 'background
		      :atlas-cell ,background
		      :sheet *areas-sheet*)

       (make-instance 'text
		      :string ,(format nil "You are ~a." (area-text name))
		      :x (+ *origin-x* 200)
		      :y (+ *origin-y* 400))
       ,(when foreground
	      `(make-instance 'foreground
			      :atlas-cell ,foreground
			      :sheet *areas-sheet*))
       (make-instance 'stencil
		      :sheet *areas-stencils*
		      :index ,stencil
		      :cell-width (area-width)
		      :cell-height (area-height)))
     ,@body
     (store-area ,id area)
     area))

(defmacro make-element ((entity-manager x y
					&key costume stencil)
			&body body)
  `(make-game-entity (,entity-manager)
       (make-instance 'bounding-box
		      :x ,x
		      :y ,y
		      :width (sprite-width)
		      :height (sprite-height))
       ,(when costume
	      `(make-instance 'costume
			      :sheet *sprites-sheet*
			      :data ,costume))
       ,(when stencil
	      `(make-instance 'stencil
			      :sheet *sprites-stencils*
			      :index ,stencil
			      :cell-width (sprite-width)
			      :cell-height (sprite-height)))
       ,@body))

(defmacro make-monster ((entity-manager x y
					&key costume stencil
					move-fun move-acrossp
					speed weapons)
			&body other-components)
  `(make-element (,entity-manager ,x ,y
				    :costume ,costume
				    :stencil ,stencil)
     (make-instance 'overlaps-with-player
		    :test #'overlaps-pixel-p
		    :action #'collide-player-monster)
     (make-instance 'move
		    :speed ,speed
		    :action ,move-fun
		    :acrossp ,move-acrossp)
     (make-instance 'vulnerability
		    :weapons ,weapons)
     ,@other-components))

(defmacro make-item ((entity-manager
		      x y
		      &key costume stencil type builder action)
		     &body other-components)
  `(make-element (,entity-manager ,x ,y
				     :costume ,costume
				     :stencil ,stencil)
     (make-instance 'overlaps-with-player
		    :test #'overlaps-pixel-p
		    :action #'pickup-item)
     (make-instance 'item
		    :builder ,builder
		    :action ,action
		    :type ,type)
     ,@other-components))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monsters

(defmacro defmonster ((name &key costume stencil (speed 50) move-fun move-acrossp weapons) &body other-components)
  (let ((fname (intern (string-upcase (format nil "MAKE-~a" name)) :sorcery-es)))
   `(defun ,fname (entity-manager &key x y)
      (make-monster (entity-manager
		     x y
		     :costume ,costume
		     :stencil ,stencil
		     :speed ,speed
		     :move-fun ,move-fun
		     :move-acrossp ,move-acrossp
		     :weapons ,weapons)
	,@other-components))))

(defmonster (ghost :costume '((:left (44 45 46 47))
			     (:front (48 49 50 51))
			     (:right (52 53 54 55)))
		   :stencil 22
		   :speed 40
		   :move-fun #'hunt-down
		   :move-acrossp t
		   :weapons '(axe fireball)))

(defmonster (eye :costume '((:front (32 33 34 35)))
		 :stencil 20
		 :move-fun #'hunt-down
		 :weapons '(flail fireball)))

(defmonster (head 
	     :costume '((:front (56 57 58 59)))
	     :stencil 23
	     :move-fun #'hunt-down
	     :weapons '(flail fireball)))

(defmonster (wildboar
	     :costume '((:front (83 84 85 86)))
	     :stencil 26
	     :move-fun #'hunt-down
	     :weapons '(flail fireball)))

(defmonster (warlock
	     :costume '((:left (72 73 74 75))
			 (:front (76 77 78))
			 (:right (79 80 81 82)))
	     :stencil 25
	     :speed 60
	     :move-fun #'hunt-down-h
	     :weapons '(sword fireball)))

;; Items

(defmacro defitem ((name &key costume stencil action))
  (let ((fname (intern (string-upcase (format nil "MAKE-~a" name)) :sorcery-es)))
   `(defun ,fname (entity-manager &key x y)
      (make-item (entity-manager
		  x y
		  :costume '((:front ,costume))
		  :stencil ,stencil
		  :builder ',fname
		  :action ,action
		  :type (quote ,name))))))

;; Weapons
(defitem (axe :costume (1) :stencil 1))
(defitem (flail :costume (4) :stencil 4))
(defitem (sword :costume (15) :stencil 15))
(defitem (shootingstar :costume (16)
		       :stencil 16
		       :action #'make-8-fireballs))
(defitem (spellbag :costume (12)
		   :stencil 12
		   :action #'make-4-fireballs))

;; Keys

(defitem (amphora :costume (0) :stencil 0))
(defitem (coatofarms :costume (2) :stencil 2))
(defitem (goldkey :costume (3) :stencil 3))
(defitem (fleurdelys :costume (5) :stencil 5))
(defitem (goblet :costume (6) :stencil 6))
(defitem (goldenchalice :costume (7) :stencil 7))
(defitem (jewelledcrown :costume (8) :stencil 8))
(defitem (littlelyre :costume (9) :stencil 9))
(defitem (magicwand :costume (10) :stencil 10))
(defitem (moon :costume (11) :stencil 11))
(defitem (spellbook :costume (13) :stencil 13))
(defitem (scroll :costume (14) :stencil 14))



;; Door
(defmacro defdoor ((name &key costume stencil (opening nil) contact-test contact-action))
  (let ((fname (intern (string-upcase (format nil "MAKE-~a" name)) :sorcery-es)))
    `(defun ,fname (entity-manager &key x y id key target)
       (make-element (entity-manager
		      x y
		      :costume ,costume
		      :stencil ,stencil)
	 (make-instance 'overlaps-forbidden)
	 (if id (make-instance 'door
			       :id id
			       :opening ,opening
			       :target target))
	 (make-instance 'contact
			:test ,contact-test
			:action ,contact-action)
	 (make-instance 'lock
			:status (if key t nil) ;; lock status
			:key key ;; unlock key type
			)))))

(defdoor (door-right :costume '((:front (24))
				(:open (24 25 26 27))
				(:close (27 26 25 24)))
		     :stencil 19
		     :opening :right
		     :contact-action #'contact-player-open-object
		     :contact-test #'contact-door-right))

(defdoor (door-left :costume '((:front (20))
			       (:open (20 21 22 23))
			       (:close (23 22 21 20)))
		    :stencil 18
		    :opening :left
		    :contact-action #'contact-player-open-object
		    :contact-test #'contact-door-left))

(defdoor (door-green :costume '((:front (93)))
		     :stencil 27
		     :contact-action #'contact-player-remove-object
		     :contact-test #'contact-door-green))

(defdoor (door-trap :costume '((:front (95)))
		    :stencil 28
		    :contact-action #'contact-player-remove-object
		    :contact-test #'contact-door-trap))

(defun make-door-right-opening (entity-manager &key x y)
  (make-element (entity-manager
		 x y
		 :costume '((:front (24 25 26 27))))))

(defun make-door-left-opening (entity-manager &key x y)
  (make-element (entity-manager
		 x y
		 :costume '((:front (20 21 22 23))))))

(defun make-pedestal (entity-manager &key x y)
  (make-element (entity-manager x y)
    (make-instance 'contact
		   :action #'victory
		   :test #'contact-pedestal)))

(defun make-cauldron (entity-manager &key x y (contact-action #'restore-energy))
  (make-element (entity-manager
		 x y
		 :costume '((:front (17 18)))
		 :stencil 17)
    (make-instance 'contact
		   :action contact-action
		   :test #'contact-cauldron)
    (make-instance 'overlaps-forbidden)))

(defun make-fire (entity-manager &key x y)
  (make-element (entity-manager
		 x y
		 :costume '((:front (98 99)))
		 :stencil 30)
    (make-instance 'overlaps-with-player
		   :test #'overlaps-pixel-p
		   :action #'drain-energy)))

(defun make-release-anim (entity-manager &key x y)
  (make-element (entity-manager
		 x y
		 :costume '((:front (40 41 42 43))))
    (make-instance 'remove-at-boundaries)
    (make-instance 'move
		   :speed 20
		   :outsidep t
		   :acrossp t
		   :action #'move-top)))

(defun make-friend (entity-manager &key x y (status t))
  (make-element (entity-manager
		 x y
		 :costume '((:front (36 37 38 39))
			    (:victory (64 65 66 67)))
		 :stencil 21)
		(when status
		  (make-instance 'overlaps-with-player
				 :test #'overlaps-pixel-p
				 :action #'release-friend))))

(defun make-waterfall (entity-manager &key x y width height)
  (let ((waterfall-pixels-width 32)
	(waterfall-pixels-height 16))
      (loop for j below height
	  do (loop for i below width
		do (make-element (entity-manager
				  (+ x (* i waterfall-pixels-width))
				  (+ y (* j waterfall-pixels-height))
				  :costume '((:front (106 107 108 109)))))))))

(defun make-wave (entity-manager &key x y)
  (make-element (entity-manager
		 x  y
		 :stencil 31
		 :costume '((:front (110 111 112 113))))
    (make-instance 'overlaps-with-player
		   :test #'overlaps-pixel-p
		   :action #'player-drown)))

(defun make-river (entity-manager &key x y size)
  (let ((wave-pixel-width 32))
      (loop for i below size
	 do (make-wave entity-manager
		       :x (+ x (* i wave-pixel-width))
		       :y y))))

(defun make-splash (entity-manager &key x y)
  (make-element (entity-manager
		 x  y
		 :costume '((:front (87 88 89 90))))))

(defun make-drown (entity-manager &key x y)
  (make-element (entity-manager
		 x  y
		 :costume '((:front (91 92))))))

(defun make-explosion (entity-manager &key x y)
  (make-element (entity-manager
		 x  y
		 :costume '((:front (28 29 30 31))))
    (make-instance 'animation :action #'remove-entity)))

(defun make-fireball (entity-manager directions &key x y)
  (make-element (entity-manager
		 x  y
		 :costume '((:front (97)))
		 :stencil 29)
    (make-instance 'item :type 'fireball)
    (make-instance 'move
		   :speed 1
		   :outsidep t
		   :acrossp t
		   :action #'(lambda (entity-manager entity)
				(dolist (direction directions)
				  (move-to entity-manager
					   entity
					   direction))))))


(defun make-4-fireballs (entity-manager &key x y)
  (make-fireball entity-manager '(:top) :x x :y y)
  (make-fireball entity-manager '(:bottom) :x x :y y)
  (make-fireball entity-manager '(:left) :x x :y y)
  (make-fireball entity-manager '(:right) :x x :y y))

(defun make-8-fireballs (entity-manager &key x y)
  (make-fireball entity-manager '(:top) :x x :y y)
  (make-fireball entity-manager '(:bottom) :x x :y y)
  (make-fireball entity-manager '(:left) :x x :y y)
  (make-fireball entity-manager '(:right) :x x :y y)
  (make-fireball entity-manager '(:top :left) :x x :y y)
  (make-fireball entity-manager '(:top :right) :x x :y y)
  (make-fireball entity-manager '(:bottom :left) :x x :y y)
  (make-fireball entity-manager '(:bottom :right) :x x :y y))

(defun make-player (state &key x y)
  (let ((energy (make-instance 'energy))
	(score (make-instance 'score))
	(release (make-instance 'release)))
    
    (setf (board-entity-components state :player)
	  (list
	   (make-instance 'costume
			  :sheet *sprites-sheet*
			  :data '((:left (60 61 62 63))
				  (:front (64 65 66 67))
				  (:right (68 69 70 71))))
	   (make-instance 'stencil
			  :sheet *sprites-stencils*
			  :index 24
			  :cell-width (sprite-width)
			  :cell-height (sprite-height))
	   (make-instance 'move :speed 10 :action #'player-keyboard-move)
	   (make-instance 'bag)
	   energy 
	   (make-instance 'text
			  :update #'(lambda (text)
				      (setf (text-string text)
					    (format nil "Energy : ~a %" (energy-amount energy))))
			  :x (+ *origin-x* 0)
			  :y (- *origin-y* 40))
	   score
	   (make-instance 'text
			  :update #'(lambda (text)
				      (setf (text-string text)
					    (format nil "Score : ~a" (score-value score))))
			  :x (+ *origin-x* 470)
			  :y (- *origin-y* 40))
	   release
	   (make-instance 'text
			  :update #'(lambda (text)
				      (setf (text-string text)
					    (format nil "Released friends : ~a/~a" (release-amount release) (release-max release))))
			  :x (+ *origin-x* 220)
			  :y (+ *origin-y* 340))
	   (make-instance 'bounding-box
			  :x x
			  :y y  
			  :width (sprite-width)
			  :height (sprite-height))))))

(defun make-timebook (state &key x y)
  (let* ((width 128)
	 (height 80)
	 (atlas-cells '((100 101 102) (103 104 105)))
	 (surface (sdl:create-surface width height)))

    (loop for j below (length atlas-cells)
       for cells in atlas-cells
       do (loop for i below (length cells)
	     for cell in cells
	     do (sdl:draw-surface-at-* (sheet *sprites-sheet*)
					 (* i (sprite-width))
					 (* j (sprite-height))
					 :surface surface
					 :cell cell)))
    (setf (board-entity-components state :timebook) 
	  (list
	   (make-instance 'countdown
			  :index (* width height)
			  :step-action #'deface-time-book
			  :end-action #'player-explose)
	   (make-instance 'costume
			  :data '((:front (0)))
			  :sheet (make-instance 'sprites-sheet
						:sheet surface
						:sprite-width width
						:sprite-height height))
	   (make-instance 'bounding-box
			  :x x
			  :y y 
			  :width width
			  :height height)))))

(defun make-bag (state)
  (setf (board-entity-components state :bag)
	(list
	 (make-instance 'costume
			:sheet *sprites-sheet*
			:data ())
	 (make-instance 'bounding-box
			:x 40 
			:y (+ (area-height) 
			      (sprite-height))
			:width (+ 4 (sprite-width))
			:height (+ 4 (sprite-height)))
	 (make-instance 'rectangle))))


(defun player-entity ()
  (board-entity-id *current-state* :player))

(defun timebook-entity ()
  (board-entity-id *current-state* :timebook))

(defun bag-entity ()
  (board-entity-id *current-state* :bag))
