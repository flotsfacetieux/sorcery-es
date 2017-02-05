(in-package :sorcery-es)

(defclass render-system (system) ())

(defun atlas-cell (costume)
  ;; return the atlas cell of the current costume
  (nth (costume-index costume)
       (cadr (assoc (costume-direction costume)
		    (costume-data costume)))))

(defun display-scenery (em type)
  (dolist (entity (find-entities em type))
    (let ((ground (entity-component em entity type)))
      (sdl:draw-surface-at-* (sheet (ground-sheet ground))
			     *origin-x*
			     *origin-y*
			     :cell (gd-atlas-cell ground)))))

(defun display-elements (em)
  (dolist (entity (find-entities em 'costume))
    (let ((costume (entity-component em entity 'costume))
	  (bounding-box (entity-component em entity 'bounding-box)))

      (when (costume-data costume)
	(sdl:draw-surface-at-* (sheet (costume-sheet costume))
			       (+ *origin-x*
				  (x bounding-box))
			       (+ *origin-y*
				  (y bounding-box))
			       :cell (atlas-cell costume))))))


(defun display-box (em)
  (dolist (entity (find-entities em 'rectangle))
    (let ((rect (entity-component em entity 'rectangle))
	  (bb (entity-component em entity 'bounding-box)))
      (sdl:draw-rectangle-* (+ *origin-x* (x bb))
			    (+ *origin-y* (y bb))
			    (width bb)
			    (height bb)
			    :color (rect-color rect)))))

(defun display-text (em)
  (dolist (text (find-components em 'text))
    (sdl:draw-string-solid-*
     (format nil "~a" (text-string text))
     (text-x text)
     (text-y text)
     :color (text-color text))))

(defmethod update ((system render-system) entity-manager)
  (sdl:draw-surface-at-* (sheet *bg-sheet*) 0 0 :cell 0)
  (when entity-manager
    (display-scenery entity-manager 'background)
    (display-elements entity-manager)
    (display-text entity-manager)
    (display-box entity-manager)
    (display-scenery entity-manager 'foreground)))
