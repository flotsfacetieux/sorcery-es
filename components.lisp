(in-package :sorcery-es)

(defcomponent area
  ((id :accessor area-id
       :initarg :id)))

(defclass rectangle (component)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (color :accessor rect-color
	  :initform sdl:*yellow*)))

(defclass bounding-box (component)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defmethod intersectp ((bb1 bounding-box) (bb2 bounding-box))
  (not (or (>= (x bb2) (+ (x bb1) (width bb1)))
	   (<= (+ (x bb2) (width bb2)) (x bb1))
	   (>= (y bb2) (+ (y bb1) (height bb1)))
	   (<= (+ (y bb2) (height bb2)) (y bb1)))))

(defmethod bounding-boxes-intersection ((bb1 bounding-box) (bb2 bounding-box))
  "Returns the bounds intersection of bb1 and bb2."
  (let ((x (max (x bb1) (x bb2)))
	(y (max (y bb1) (y bb2))))
    (make-instance 'bounding-box
		   :x x
		   :y y
		   :width (- (min (+ (x bb1) (width bb1))
				  (+ (x bb2) (width bb2)))
			     x)
		   :height (- (min (+ (y bb1) (height bb1))
				   (+ (y bb2) (height bb2)))
			      y))))

(defclass contact (component)
  ((test :accessor contact-test
	 :initarg :test)
   (action :accessor contact-action
	   :initarg :action)))

(defclass overlaps-with-player (component)
  ((test :accessor overlaps-test
	 :initarg :test)
   (action :accessor overlaps-action
	   :initarg :action)))

(defclass overlaps-forbidden (component)
  ())

(defclass costume (component)
  ((sheet :accessor costume-sheet
	  :initarg :sheet)
   (index :accessor costume-index
	  :initform 0)
   (direction :accessor costume-direction
	      :initform :front)
   (data :accessor costume-data
	 :initarg :data)))

(defmethod costume-sprites-count ((costume costume))
  (let ((direction (or (costume-direction costume) :front)))
    (length (cadr (assoc direction (costume-data costume))))))

(defclass ground (component)
  ((sheet :accessor ground-sheet
	  :initarg :sheet)
   (atlas-cell :accessor gd-atlas-cell
	       :initarg :atlas-cell)))

(defclass foreground (ground)
  ())

(defclass background (ground)
  ())

(defclass stencil (component)
  ((sheet :accessor stencil-sheet
	  :initarg :sheet)
   (index :accessor stencil-index
	  :initarg :index)
   (cell-width :accessor stencil-cell-width
	       :initarg :cell-width)
   (cell-height :accessor stencil-cell-height
		:initarg :cell-height)))

(defmethod read-bit ((stencil stencil) x y)
  (stencil-read-bit (stencil-sheet stencil)
		    (stencil-index stencil)
		    x y
		    (stencil-cell-width stencil)
		    (stencil-cell-height stencil)))

(defclass countdown (component)
  ((index :accessor countdown-index
	  :initarg :index)
   (step-action :accessor countdown-step-action
		:initarg :step-action)
   (end-action :accessor countdown-end-action
	       :initarg :end-action)))

(defclass item (component)
  ((type :accessor item-type
	 :initarg :type)
   (action :accessor item-action
	   :initarg :action
	   :initform nil)
   (builder :accessor item-builder
	    :initarg :builder)))

(defclass bag (component)
  ((item :accessor bag-item
	 :initform nil)))

(defclass vulnerability (component)
  ((weapons :accessor weapons
	    :initarg :weapons
	    :initform '(fireball))))

(defclass energy (component)
  ((dt-acc :accessor energy-dt-acc
	   :initform 0)
   (dt-inc :accessor energy-dt-inc
	   :initform 400)
   (dt-dec :accessor energy-dt-dec
	   :initform 1200)
   (amount :accessor energy-amount
	   :initform 99)))

(defmethod increase-energy ((energy energy))
  (incf (energy-amount energy))
  (setf (energy-dt-acc energy) 0))

(defmethod decrease-energy ((energy energy))
  (decf (energy-amount energy))
  (setf (energy-dt-acc energy) 0))

(defclass move (component)
  ((dt-acc :accessor move-dt-acc
	   :initform 0)
   (speed :accessor move-speed
	  :initform 60
	  :initarg :speed)
   (stack :accessor move-stack
	  :initform '())
   (outsidep :accessor move-outsidep
	     :initarg :outsidep
	     :initform nil)
   (acrossp :accessor move-acrossp
	    :initarg :acrossp
	    :initform nil)
   (action :accessor move-action
	   :initarg :action
	   :initform nil)))

(defmethod remove-direction ((move move) direction)
  (setf (move-stack move) (remove direction (move-stack move))))

(defmethod reset-move-stack ((move move))
  (setf (move-stack move) '()))

(defmethod reset-move-cycle ((move move))
  (reset-move-stack move)
  (setf (move-dt-acc move) 0))

(defclass door (component)
  ((id :accessor door-id
       :initarg :id)
   (target :accessor door-target
	   :initarg :target)
   (opening :accessor door-opening
	    :initarg :opening)))

(defclass lock (component)
  ((status :accessor lock-status
	   :initarg :status)
   (key :accessor lock-key
	:initarg :key)
   (action :accessor unlock-action
	   :initarg :action)))

(defmethod unlockp ((lock lock) key)
  (or (not (lock-status lock))
      (when (equal key (lock-key lock))
	(setf (lock-status lock) nil))
      (not (lock-status lock))))

(defmethod lockedp ((lock lock))
  (lock-status lock))

(defclass text (component)
  ((string :accessor text-string
	   :initarg :string)
   (update :accessor text-update
	   :initarg :update
	   :documentation "function to update string")
   (color :accessor text-color
	  :initform sdl:*yellow*)
   (x :accessor text-x
      :initarg :x)
   (y :accessor text-y
      :initarg :y)))

(defmethod text-pixel-width ((text text))
  (sdl:get-font-size (text-string text) :size :w))

(defclass roll-text (component)
  ((x-left :accessor x-left
	   :initarg :x-left)
   (x-right :accessor x-right
	    :initarg :x-right)
   (step :accessor rlt-step
	 :initarg :step)))

(defclass animation (component)
  ;; used to send an action when all the sprites of the costume are displayer once.
  ;; Example : remove object after explosion
  ((action :accessor animation-action
	   :initarg :action)))

(defclass remove-at-boundaries (component)
  ;; used to remove object when it's positions
  ;; is over the game board (ex : fireballs)
  ())

(defclass score (component)
  ((name :accessor score-name
	 :initarg :name)
   (value :accessor score-value
	  :initform 0
	  :initarg :value)))

(defclass release (component)
  ((amount :accessor release-amount
	   :initform 0
	   :documentation "Amount of released friends")
   (max :accessor release-max
	:initform 8
	:documentation "Nb of friends to release")))
