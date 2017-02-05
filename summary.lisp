(in-package :sorcery-es)

(defun fill-line-size (str-name str-value)
  (let ((text-width (sdl:get-font-size
		     (format nil "~a~a"
			     str-name
			     str-value)
			   :size :w))
	(point-char-width (sdl:get-font-size "." :size :w)))
    (truncate
     (/ (- *area-width* text-width) point-char-width))))

(defun summary-text-line (str-name str-value)
  (format nil "~a~v,,,v<~>~a"
	  str-name
	  (fill-line-size str-name str-value)
	  #\.
	  str-value))

(defun init-summary (state score released-friends time)
  (let ((x *canvas-middle-x*)
	(title "SUMMARY")
	(entity-manager (make-instance 'entity-manager)))
    (setf (gethash :summary *entities*) entity-manager)
    (make-game-entity (entity-manager)
      (make-instance 'text
		     :x (- x (/ (sdl:get-font-size title
						   :size :w)
				2))
		     :y (+ *origin-y* 10)
		     :string title)
      (make-instance 'text
		     :x *origin-x* 
		     :y (+ *origin-y* 100)
		     :string (summary-text-line "Score : " score))
      (make-instance 'text
		     :x *origin-x* 
		     :y (+ *origin-y* 140)
		     :string (summary-text-line "Elapsed Time : "
						(format nil "~a s"
							time)))
      (make-instance 'text
		     :x *origin-x* 
		     :y (+ *origin-y* 180)
		     :string (summary-text-line
			      "Released Friends : "
			      released-friends)))
    (make-game-entity (entity-manager)
      (make-instance 'roll-text
		    :step 2
		    :x-left 0
		    :x-right *canvas-x*)
      (make-instance 'text
		    :string "Press 'r' to return"
		    :x *canvas-x*
		    :y (+ *origin-y* 360)))

    (setf (state-em state) entity-manager)
    (switch-state :summary)))

(defun summary-state ()
  (make-instance
   'state
   :data (list :system (make-instance 'roll-text-system))
   :engine #'(lambda (state)
	       (let ((em (state-em state)))
		 (update (getf (state-data state) :system) em)))
   :keyboard
   (list :down
	 (list
	  (cons :sdl-key-q
		#'(lambda () (sdl:push-quit-event)))
	  (cons :sdl-key-r
		#'(lambda ()
		    (init-game (dict-state :game))
		    (cheat-code-reset)
		    (switch-state :intro)))))))

 
