(in-package :sorcery-es-stencils-builder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rbtest*
  (make-array (* 3 2 3)
	      :element-type 'bit
	      :initial-contents
	      '(0 0 0
		1 0 1
		1 1 1
		0 1 0
		1 1 0
		0 0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read function
(defun stencil-read-bit (seq cell x y w h)
  (sbit seq (+ x (* y w) (* cell w h))))

(defun display-stencil-data (seq cell w h)
  (dotimes (y h)
    (dotimes (x w)
      (format t "~a" (stencil-read-bit seq cell x y w h)))
    (format t "~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transparent-color-p (x y)
  ;; transparent color is black
  (sdl:color= sdl:*black*
	      (sdl:read-pixel-* x y
				:surface sdl:*default-display*)))

(defun stencil-from (x-start y-start w h)
  ;; return an array of bits (one dimension) which represents the stencil of the default sdl surface
  ;; 0 means the pixel of the surface is of transparent color
  ;; 1 means the pixel of the surface is not ot transparent color
  ;; x-start and y-start : coords of the pixels area
  ;; w and h : size of the pixels area
  (let ((stencil (make-array (* h w) :element-type 'bit)))
    (loop for y below h
	 do (loop for x below w
	       do (setf (sbit stencil (+ x (* y w )))
			(if (transparent-color-p
			     (+ x x-start)
			     (+ y y-start))
			    0 1))))
    stencil))

(defun draw-sprites-for-one-stencil (atlas cell-indexes x y)
  ;; draw sprites for one stencil at position x y
  ;; atlas : sprites sheet
  ;; cell-indexes : indexes of the sprites used to build stencil
  (sdl:clear-display sdl:*black*)
  (mapcar #'(lambda (cell-index)
	      (sdl:draw-surface-at-* atlas x y :cell cell-index))
	  (alexandria:ensure-list cell-indexes))
  (sdl:update-display))

(defun build-stencils-file () 
  (let ((x 10)
	(y 10)
	(area-cells '(0 1 2 4 6 8 10 12 14 16 18 20
		      22 23 25 27 29 31 33 34 36 38 40 42
		      44 46 48 50 51 53 55 57 59 60 61 63 65
		      66 68 69))
	(sprite-cells '(0 1 2 3 4 5 6 7 8 9 10 11
			12 13 14 15 16 19 20 24
			(32 33 34 35) ;; eye - 20
			(36 37 38 39) ;; friend -21
			(44 45 46 47 48 49 50 51 52 53 54 55) ;;ghost  22
			(56 57 58 59) ;; head - 23
			(60 61 62 63 64 65 66 67 68 69 70 71) ;; player 24
			(72 73 74 75 76 77 78 79 80 81 82) ;; warlock 25
			(83 84 85 86) ;; wildboar - 26
			94 96 97 ;; 27, 28, 29
			(98 99) ;; fire - 30
			(110 111 112 113) ;; waves - 31
			)))
    (setf *zoom-scale* 1)
    (sdl:with-init ()
      (sdl:window 640 288
		  :bpp 32 :any-format t :double-buffer t)
      (load-graphics)
      (with-open-file (s *sprites-stencils-pathname*
			 :direction :output
			 :if-exists :supersede
			 :element-type '(unsigned-byte 8))
	(mapcar #'(lambda (data)
		    (draw-sprites-for-one-stencil
		     (sheet *sprites-sheet*)
		     data x y)
		    (write-sequence (stencil-from x y 48 48) s))
		sprite-cells))
      (with-open-file (s *areas-stencils-pathname* 
			 :direction :output
			 :if-exists :supersede
			 :element-type '(unsigned-byte 8))
	(mapcar #'(lambda (data)
		    (draw-sprites-for-one-stencil
		     (sheet *areas-sheet*)
		     data x y)
		    (write-sequence (stencil-from x y 640 288) s))
		area-cells))
      )))
