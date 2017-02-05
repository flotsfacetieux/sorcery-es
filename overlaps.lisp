(in-package :sorcery-es)

(defun inside-boundaries-p (bounding-box)
  (and
   (< (x bounding-box)
      (- (area-width) (width bounding-box)))
   (>= (x bounding-box) 0)
   (< (y bounding-box)
      (- (area-height) (height bounding-box)))
   (>= (y bounding-box) 0)))

(defun overlaps-pixel-p (entity-manager entity1 bb1 entity2 bb2)
  ;; Overlaps tests are done with stencils
  (let* ((bb-intersection (bounding-boxes-intersection bb1 bb2))
	 (stencil1 (entity-component entity-manager entity1 'stencil))
	 (stencil2 (entity-component entity-manager entity2 'stencil))
	 (x1 (- (x bb-intersection) (x bb1)))
	 (y1 (- (y bb-intersection) (y bb1)))
	 (x2 (- (x bb-intersection) (x bb2)))
	 (y2 (- (y bb-intersection) (y bb2))))

    (when (and stencil1 stencil2)
      (dotimes (yindex (height bb-intersection))
	(dotimes (xindex (width bb-intersection))
	  (when (= (read-bit stencil1 (+ xindex x1) (+ yindex y1))
		   (read-bit stencil2 (+ xindex x2) (+ yindex y2))
		   1)
	    (return-from overlaps-pixel-p t)))))))

(defun background-overlapsp (entity-manager entity next-bounding-box)
  (let ((bg-entity (car (find-entities entity-manager 'background)))
	(bg-bb (make-instance 'bounding-box :x 0 :y 0 :width (area-width) :height (area-height))))
    (overlaps-pixel-p entity-manager bg-entity bg-bb entity next-bounding-box)))

(defun overlapsp (entity-manager entity next-bounding-box)
  ;; return true if entity overlaps another entity with a component of type 'overlaps-forbidden'

  (or  (background-overlapsp entity-manager entity next-bounding-box)
	(dolist (elmt (find-entities entity-manager 'overlaps-forbidden))
	  (let ((elmt-bb (entity-component entity-manager elmt 'bounding-box)))
	    (when (intersectp next-bounding-box elmt-bb)
	      (when (overlaps-pixel-p entity-manager entity next-bounding-box elmt elmt-bb)
		(return-from overlapsp t)))))))
