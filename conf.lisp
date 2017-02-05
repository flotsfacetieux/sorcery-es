(in-package :sorcery-es)

(defparameter *cheat-mode* nil)
(defparameter *pause-flag* nil)
(defparameter *swap-door* nil)
(defparameter *swap-area-switch* nil)

(defparameter *sprites-pathname*
  (merge-pathnames "data/sprites-48x48.gif" *base-directory*))

(defparameter *sprites-stencils-pathname*
  (merge-pathnames "data/sprites-stencils.bin" *base-directory*))

(defparameter *areas-pathname*
  (merge-pathnames "data/areas-48x48.gif" *base-directory*))

(defparameter *areas-stencils-pathname*
  (merge-pathnames "data/areas-stencils.bin" *base-directory*))

(defparameter *splash-pathname*
  (merge-pathnames "data/splash-screen-48x48.gif" *base-directory*))

(defparameter *bg-pathname*
  (merge-pathnames "data/sorcery.gif" *base-directory*))

(defparameter *zoom-scale* 1)
(defparameter *origin-x* 12)
(defparameter *origin-y* 100)
(defparameter *area-width* 640)
(defparameter *area-height* 288)
(defparameter *sprite-width* 48)
(defparameter *sprite-height* 48)

(defun sprite-width ()
  (* *sprite-width* *zoom-scale*))

(defun sprite-height ()
  (* *sprite-height* *zoom-scale*))

(defun area-width ()
  (* *area-width* *zoom-scale*))

(defun area-height  ()
  (* *area-height* *zoom-scale*))

(defparameter *sprites-stencils*
  (make-array (* (sprite-width)
		 (sprite-height)
		 32)
	      :element-type 'bit))
(defparameter *areas-stencils*
  (make-array (* (area-width)
		 (area-height)
		 40)
	      :element-type 'bit))

(defvar *entities*)

(defvar *sprites-sheet*)
(defvar *areas-sheet*)
(defvar *bg-sheet*)
(defvar *canvas-middle-x*)
(defvar *canvas-x*)

(defvar *current-area*)

(defvar *game-start-time*)

(defvar *player-fire-switch*)
(defparameter *player-keyboard-stack* ())

(defvar *released-friends-positions*)
