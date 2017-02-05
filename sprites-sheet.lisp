(in-package :sorcery-es)

;; Building Texture Atlas

(defun image-cells (image-width image-height sprite-width sprite-height)
  (loop for y from 0 to (- image-height sprite-height) by sprite-height
     append (loop for x from 0 to (- image-width sprite-width) by sprite-width
	       collect (list x y sprite-width sprite-height))))

(defclass sprites-sheet ()
  ((cells :reader cells)
   (sheet :accessor sheet
	  :initarg :sheet)
   (sprite-width :accessor sheet-sprite-width
		 :initarg :sprite-width)
   (sprite-heigth :accessor sheet-sprite-height
		  :initarg :sprite-height)))

(defmethod initialize-instance :after ((sprites-sheet sprites-sheet) &key)
  (let* ((sheet (sheet sprites-sheet))
	 (img-w (sdl:width sheet))
	 (img-h (sdl:height sheet))
	 (cells (image-cells img-w
			     img-h
			     (sheet-sprite-width sprites-sheet)
			     (sheet-sprite-height sprites-sheet))))
    (setf (slot-value sprites-sheet 'cells) cells)
    (setf (sdl:cells sheet) cells)))

(defun make-sprites-sheet (filename sprite-width sprite-height)
  (make-instance 'sprites-sheet
		 :sheet (sdl:load-image filename
					:color-key sdl:*black*)
		 :sprite-width sprite-width
		 :sprite-height sprite-height))
