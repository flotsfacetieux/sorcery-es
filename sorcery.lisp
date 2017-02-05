(in-package :sorcery-es)

(load-stencils)

(defun canvas-x ()
  (setf *canvas-x*
	(aref (sdl:video-dimensions) 0)))

(defun canvas-middle-x ()
  (setf *canvas-middle-x*
	(/ *canvas-x* 2)))

(defun canvas-origin-x ()
  (setf *origin-x*
	(/ (-  *canvas-x* *area-width*) 2)))

(defun load-graphics ()
  (setf *bg-sheet*
	(make-sprites-sheet *bg-pathname*
			    800
			    600))
  (setf *sprites-sheet*
	(make-sprites-sheet *sprites-pathname*
			    (sprite-width)
			    (sprite-height)))
  (setf *areas-sheet*
	(make-sprites-sheet *areas-pathname*
			    (area-width)
			    (area-height))))

(let (areas)
  (defun areas-id ()
    (when (not areas)
      (setf areas
	    (loop for k being the hash-keys in *entities*
	       collect k)))
    areas))

(defun next-area-id (area-id)
  (let ((areas-ids (areas-id)))
    (when-let ((position (position area-id areas-ids)))
      (if (= position (1- (length areas-ids)))
	  (car areas-ids)
	  (nth (1+ position) areas-ids)))))

(defun previous-area-id (area-id)
  (let ((areas-ids (areas-id)))
    (when-let ((position (position area-id areas-ids)))
      (if (= position 0)
	  (last areas-ids)
	  (nth (1- position) areas-ids)))))

(defun area-entities (area-id)
  (gethash area-id *entities*))

(defun start ()
  (sdl:with-init ()
    (sdl:window 800
		600
		:title-caption "Sorcery"
		:bpp 32 :any-format t :double-buffer t
		:fps (make-instance 'sdl:fps-timestep)
;;		(make-instance 'sdl:fps-fixed)
		)
    (setf *entities* (make-hash-table))
    (load-graphics)
    (canvas-x)
    (canvas-origin-x)
    (canvas-middle-x)
    (sdl:initialise-default-font sdl:*font-10x20*)
    (setf (sdl:frame-rate) 60)

    (setf (game-render-system) (make-instance 'render-system))
    (setf (game-costume-system) (make-instance 'costume-system))
    (setf (dict-state :summary) (summary-state))
    (setf (dict-state :game) (game-state))
    (setf (dict-state :intro) (intro-state))

    (init-intro (dict-state :intro))

    (sdl:update-display)
    (sdl:enable-key-repeat 500 20)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (key-event *current-state* :down key))
      (:key-up-event (:key key)
		     (key-event *current-state* :up key))
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (run-state *current-state*)
	     (sdl:update-display)))))
