(in-package :sorcery-es)

(defparameter *items-cells* '(amphora 0 axe 1 coatofarms 2
			      goldkey 3 flail 4 fleurdelys
			      5 goblet 6 goldenchalice 7
			      jewelledcrown 8 littlelyre 9
			      magicwand 10 moon 11 spellbag 12
			      spellbook 13 scroll 14 sword 15
			      shootingstar 16))

(defun start-area ()
  (let ((areas '((:L1 (384 2)) (:V3 (272 2)) (:P4 (272 2))
		 (:I2 (520 44)) (:O6 (534 100)))))
    (random-elt areas)))

(defun init-released-friends-positions ()
  ;; friends position at M6 area
  (setf *released-friends-positions*
	'((12 . 212) (556 . 212) (76 . 180) (492 . 180)
	  (140 . 148) (428 . 148) (204 . 116) (364 . 116))))

(defun item-atlas-cell (item-type)
  (getf *items-cells* item-type))

(defun init-player (state pos)
  (setf *player-fire-switch* nil)
  (make-bag state)

  (make-timebook state
		 :x (- (area-width) (* 3 (sprite-width)))
		 :y (+ (/ (sprite-height) 2) (area-height)))
  (make-player state
	       :x (first pos)
	       :y (second pos)))

(defun init-areas ()
  ;; E1
  (make-area (area :E1 
		   :name "stonehenge"
		   :background 0
		   :stencil 0)
    (make-ghost area :x 48 :y 224)
    (make-axe area :x 96 :y 224)
    (make-amphora area :x 0 :y 96)
    (make-fire area :x 408 :y 208)
    (make-fire area :x 156 :y 224)
    (make-friend area :x 16 :y 16)
    (make-door-right area :x 592 :y 224
		     :id :E1F1 :target :F1)
    (make-door-green area :x 64 :y 16 :key 'moon)
    )
  ;; F1
  (make-area (area :F1
		   :name "nearStonehenge"
		   :background 1
		   :stencil 1)
    (make-wildboar area :x 176 :y 224)
    (make-sword area :x 282 :y 192)
    (make-flail area :x 334 :y 224)
    (make-fire area :x 232 :y 144)
    (make-fire area :x 536 :y 16)
    (make-door-left area :x 0 :y 224
		    :id :e1f1 :target :e1)
    (make-door-right area :x 576 :y 224
		     :id :f1g1 :target :g1)
    (make-door-right area :x 592 :y 128
		     :id :f1g2 :target :g2))
  ;; G1
  (make-area (area :G1
		   :name "tunnelMouth"
		   :background 2
		   :foreground 3
		   :stencil 2)
    (make-fire area :x 168 :y 112)
    (make-fire area :x 424 :y 112)
    (make-warlock area :x 320 :y 224)
    (make-wildboar area :x 96 :y 40)
    (make-amphora area :x 416 :y 0)
    (make-goldkey area :x 424 :y 224)
    (make-door-left area :x 16 :y 224 :id :f1g1 
    	       :target :f1)
    (make-door-right area :x 576 :y 0 :id :g1g2 
    		:target :g2)
    (make-door-right area :x 592 :y 224 :id :g1h1 
    		:target :h1))
  ;; G2
  (make-area (area :G2
		   :background 4
		   :foreground 5
		   :stencil 3
		   :name "wastelands")
    (make-warlock area :x 0 :y 128)
    (make-cauldron area :x 480 :y 128)
    (make-amphora area :x 80 :y 128)
    (make-spellbag area :x 460 :y 224)
    (make-fire area :x 248 :y 128)
    (make-fire area :x 360 :y 128)
    (make-river area :x 112 :y 240 :size 7) ; :y 136
    (make-door-trap area :x 304 :Y 160 :key 'amphora)
    (make-door-left area :x 0 :y 224 :id :f1g2 
    	       :target :f1)
    (make-door-left area :x 384 :y 224 :id :g1g2 
    	       :target :g1)
    (make-door-right area :x 592 :y 128 :id :g2h2u 
    		:target :h2)
    (make-door-right area :x 592 :y 224 :id :g2h2d 
    		:target :h2)
    )

  ;; H1
  (make-area (area :H1
		   :background 6
		   :foreground 7
		   :stencil 4
		   :name "tunnelMouth")
    (make-wildboar area :x 272 :y 224)
    (make-wildboar area :x 420 :y 224)
    (make-goldkey area :x 28 :y 48)
    (make-spellbag area :x 516 :y 224)
    (make-door-left area :x 16 :y 224 :id :g1h1 
    	       :target :g1)
    (make-door-right area :x 592 :y 32 :id :h1i1u 
    		:target :i1)
    (make-door-right area :x 592 :y 224 :id :h1i1d 
    		:target :i1)
    )

  (make-area (area :H2
		   :background 8
		   :foreground 9
		   :stencil 5
		   :name "wastelands")
    (make-ghost area :x 368 :y 0)
    (make-axe area :x 432 :y 224)
    (make-sword area :x 372 :y 128)
    (make-friend area :x 16 :y 0)
    (make-door-green area :x 64 :y 0 :key 'goblet)
    (make-door-left area :x 0 :y 224 :id :g2h2u 
    	       :target :g2)
    (make-door-left area :x 208 :y 224 :id :g2h2d 
    	       :target :g2)
    (make-door-right area :x 592 :y 112 :id :h2i2u 
    		:target :i2)
    (make-door-right area :x 592 :y 224 :id :h2i2d 
    		:target :i2))

  (make-area (area :I1
		   :background 10
		   :foreground 11
		   :stencil 6
		   :name "tunnel")
    (make-wildboar area :x 538 :y 112)
    (make-warlock area :x 352 :y 32)
    (make-goldkey area :x 312 :y 32)
    (make-goblet area :x 532 :y 224)
    (make-cauldron area :x 288 :y 224)
    (make-door-trap area :x 208 :y 64 :key 'amphora)
    (make-door-green area :x 448 :y 224 :key 'goldkey)
    (make-door-left area :x 0 :y 32 :id :h1i1u 
    	       :target :h1)
    (make-door-left area :x 0 :y 224 :id :h1i1d 
    	       :target :h1)
    (make-door-right area :x 592 :y 32 :id :i1j1 
    		:target :j1)
    (make-river area :x 472 :y 144 :size 5)
    (make-waterfall area :x 480 :y 0 :width 2 :height 5))

  (make-area (area :I2
		   :background 12
		   :foreground 13
		   :stencil 7
		   :name "wastelands")
    (make-wildboar area :x 440 :y 224)
    (make-eye area :x 288 :y 8)
    (make-shootingstar area :x 32 :y 16)
    (make-jewelledcrown area :x 368 :y 224)
    (make-door-left area :x 0 :y 208 :id :h2i2u 
    	       :target :h2)
    (make-door-left area :x 240 :y 240 :id :h2i2d 
    	       :target :h2)
    (make-door-right area :x 592 :y 32 :id :i2j3 
    		:target :j3)
    (make-door-right area :x 592 :y 224 :id :i2k2 :key
    		'coatofarms :target :k2))

  (make-area (area :J1
		   :background 14
		   :foreground 15
		   :stencil 8
		   :name "tunnel")
    (make-warlock area :x 396 :y 224)
    (make-wildboar area :x 88 :y 128)
    (make-moon area :x 576 :y 96)
    (make-coatofarms area :x 32 :y 208)
    (make-door-trap area :x 576 :y 32 :key 'goldkey)
    (make-door-left area :x 0 :y 32 :id :i1j1
		    :target :i1)
    (make-door-right area :x 592 :y 224 :id :j1k1
		     :key 'coatofarms :target :k1)
    (make-river area :x 0 :y 240 :size 10)
    (make-river area :x 0 :y 144 :size 6)
    (make-waterfall area :x 96 :y 192 :width 3 :height 5)
    )

  (make-area (area :J3
		   :background 16
		   :foreground 17
		   :stencil 9
		   :name "outsideCastle")
    (make-warlock area :x 224 :y 224)
    (make-axe area :x 80 :y 224)
    (make-sword area :x 32 :y 112)
    (make-door-left area :x 0 :y 224 :id :i2j3 
    	       :target :i2)
    (make-door-right area :x 592 :y 32 :id :j3k3 
    		:target :k3)
    (make-river area :x 320 :y 240 :size 5))

  (make-area (area :K1
		   :background 18
		   :foreground 19
		   :stencil 10
		   :name "strongroom")
    (make-warlock area :x 268 :y 224)
    (make-warlock area :x 268 :y 0)
    (make-friend area :x 272 :y 144)
    (make-amphora area :x 32 :y 112)
    (make-spellbag area :x 512 :y 112)
    (make-door-trap area :x 432 :y 16 :key 'goldkey)
    (make-door-green area :x 312 :y 144 :key 'jewelledcrown)
    (make-door-left area :x 0 :y 0
	       :id :k2k1 :target :k2)
    (make-door-left area :x 0 :y 224
		    :id :j1k1 :target :j1)
    (make-door-right area :x 592 :y 224
		     :id :k1l1 :target :l1)
    )
  (make-area (area :K2
		   :background 20
		   :foreground 21
		   :stencil 11
		   :name "dungeons")
    (make-warlock area :x 488 :y 224)
    (make-warlock area :x 488 :y 128)
    (make-sword area :x 592 :y 64)
    (make-goldkey area :x 88 :y 0)
    (make-door-trap area :x 272 :y 144 :key 'amphora)
    (make-door-left area :x 32 :y 224 :id :i2k2 
    	       :target :i2)
    (make-door-right area :x 576 :y 128 :id :k2l2 
    		:target :l2)
    (make-door-right area :x 592 :y 224 :id :k2k1 
    		:target :k1)
    (make-river area :x 0 :y 48 :size 11)
    )

  (make-area (area :K3
		   :background 22
		   :stencil 12
		   :name "castle")
    (make-flail area :x 320 :y 208)
    (make-spellbag area :x 528 :y 176)
    (make-warlock area :x 280 :y 224)
    (make-head area :x 204 :y 128)
    (make-cauldron area :x 192 :y 224)
    (make-door-green area :x 480 :y 176 :key 'amphora)
    (make-door-left area :x 0 :y 32 :id :j3k3  :target
    	       :j3)
    (make-door-right area :x 592 :y 0 :id :k3l3 
		     :target :l3)
    )

  (make-area (area :L1
		   :background 23
		   :foreground 24
		   :stencil 13
		   :name "tunnel")
    (make-sword area :x 16 :y 64)
    (make-spellbook area :x 560 :y 16)
    (make-warlock area :x 240 :y 224)
    (make-warlock area :x 396 :y 224)
    (make-cauldron area :x 592 :y 160)
    (make-door-trap area :x 192 :y 160 :key 'coatofarms)
    (make-door-green area :x 544 :y 128 :key 'goldkey)
    (make-door-left area :x 0 :y 224 :id :k1l1 
    	       :target :k1)
    (make-door-right area :x 592 :y 224 :id :l1m1 
    		:target :m1)
    )

  (make-area (area :L2
		   :background 25
		   :foreground 26
		   :stencil 14
		   :name "dungeons")
    (make-ghost area :x 268 :y 140)
    (make-axe area :x 592 :y 160)
    (make-amphora area :x 208 :y 224)
    (make-head area :x 268 :y 8)
    (make-cauldron area :x 320 :y 224)
    (make-fire area :x 8 :y 112)
    (make-friend area :x 512 :y 160)
    (make-door-trap area :x 112 :y 48 :key 'goldkey)
    (make-door-green area :x 464 :y 128 :key 'scroll)
    (make-door-left area :x 0 :y 0 :id :l3l2  :target
    	       :l3)
    (make-door-left area :x 32 :y 224 :id :k2l2 
    	       :target :k2)
    (make-door-right area :x 592 :y 224 :id :l2m3 
    		:target :m3)
    (make-river area :x 448 :y 64 :size 6)
    )

  (make-area (area :L3
		   :background 27
		   :foreground 28
		   :stencil 15
		   :name "castle")
    (make-ghost area :x 276 :y 96)
    (make-scroll area :x 176 :y 208)
    (make-coatofarms area :x 432 :y 208)
    (make-door-left area :x 0 :y 16 :id :k3l3
		    :target :k3)
    (make-door-left area :x 16 :y 160 :id :l3l2
		    :key 'coatofarms :target :l2)
    (make-door-right area :x 592 :y 16 :id :l3m3 
		     :target :m3))

  (make-area (area :M1
		   :background 29
		   :foreground 30
		   :stencil 16
		   :name "tunnel")
    (make-ghost area :x 312 :y 0)
    (make-jewelledcrown area :x 260 :y 16)
    (make-coatofarms area :x 48 :y 224)
    (make-warlock area :x 312 :y 224)
    (make-fire area :x 424 :y 32)
    (make-door-trap area :x 64 :y 160 :key 'goldkey)
    (make-door-left area :x 0 :y 144
		    :id :l1m1 :target :l1)
    (make-door-right area :x 592 :y 208 :id :m1n1 
    		:target :n1)
    (make-river area :x 448 :y 240 :size 6)
    )

  (make-area (area :M3
		   :background 31
		   :foreground 32
		   :stencil 17
		   :name "outsideCastle")
    (make-warlock area :x 470 :y 224)
    (make-eye area :x 470 :y 32)
    (make-amphora area :x 256 :y 192)
    (make-goldkey area :x 376 :y 224)
    (make-door-left area :x 0 :y 0
		    :id :l3m3 :target :l3)
    (make-door-left area :x 112 :y 144 :id :l2m3 :key 'coatofarms
    	       :target :l2)
    (make-door-right area :x 592 :y 0 :id :m3n6 
    		:target :n6)
    (make-door-right area :x 592 :y 224 :id :m3n3 
    		:target :n3)
    )
  (make-area (area :M6
		   :background 33
		   :stencil 18
		   :name "sanctuary")
    (make-pedestal area :x 288 :y 100)
    (make-door-right area :x 592 :y 0
		     :id :m6n6 :target :n6))

  (make-area (area :N1
		   :background 34
		   :foreground 35
		   :stencil 19
		   :name "tunnel")
    (make-ghost area :x 196 :y 0)
    (make-sword area :x 212 :y 208)
    (make-amphora area :x 324 :y 32)
    (make-warlock area :x 400 :y 224)
    (make-fire area :x 104 :y 64)
    (make-fire area :x 472 :y 64)
    (make-friend area :x 288 :y 112)
    (make-door-green area :x 352 :y 96 :key 'magicwand)
    (make-door-left area :x 0 :y 160 :id :m1n1
		    :target :m1)
    (make-door-right area :x 592 :y 208
		     :id :n1o1 :target :o1)
    (make-river area :x 0 :y 192 :size 6)
    (make-river area :x 96 :y 240 :size 8)
    (make-waterfall area :x 96 :y 240
		    :width 3 :height 2))

  (make-area (area :N3
		   :background 36
		   :foreground 37
		   :stencil 20
		   :name "waterfall")
    (make-littlelyre area :x 376 :y 48)
    (make-flail area :x 128 :y 48)
    (make-ghost area :x 312 :y 16)
    (make-eye area :x 312 :y 192)
    (make-door-left area :x 0 :y 208 :id :m3n3 
    	       :target :m3)
    (make-door-left area :x 0 :y 32 :id :n3n5l 
    	       :target :n5)
    (make-door-right area :x 544 :y 16 :id :n3n5r 
    		:target :n5)
    (make-door-right area :x 592 :y 208 :id :n3o3 
    		:target :o3)
    (make-river area :x 0 :y 240 :size 10)
    (make-river area :x 288 :y 64 :size 3)
    (make-river area :x 384 :y 240 :size 8)
    (make-waterfall area :x 224 :y 0 :width 2 :height
    		     7)
    (make-waterfall area :x 320 :y 112 :width 2 :height 11)
    )

  (make-area (area :N5
		   :background 38
		   :foreground 39
		   :stencil 21
		   :name "waterfall")
    (make-amphora area :x 160 :y 144)
    (make-goldkey area :x 332 :y 176)
    (make-ghost area :x 328 :y 16)
    (make-head area :x 128 :y 16)
    (make-door-left area :x 0 :y 0 :id :n5n6
		    :target :n6)
    (make-door-left area :x 0 :y 224 :id :n3n5l 
    	       :target :n3)
    (make-door-right area :x 592 :y 0 :id :n5o5 
    		:target :o5)
    (make-door-right area :x 592 :y 224 :id :n3n5r 
    		:target :n3)
    (make-waterfall area :x 224 :y 0 :width 2 :height 18))

  (make-area (area :N6
		   :background 40
		   :foreground 41
		   :stencil 22
		   :name "nearThePalace")
    (make-shootingstar area :x 96 :y 108)
    (make-spellbag area :x 480 :y 128)
    (make-door-left area :x 96 :y 0 :id :m6n6  :target
    	       :m6)
    (make-door-left area :x 0 :y 240 :id :n5n6 
    	       :target :n5)
    (make-door-right area :x 592 :y 0 :id :n6o6 
    		:target :o6)
    (make-door-right area :x 464 :y 240 :id :m3n6 
    		:target :m3)
    (make-river area :x 64 :y 48 :size 7)
    (make-waterfall area :x 0 :y 0 :width 2 :height 6)
    (make-waterfall area :x 224 :y 96 :width 2 :height 12))

  (make-area (area :O1
		   :background 42
		   :foreground 43
		   :stencil 23
		   :name "tunnel")
    (make-goldkey area :x 196 :y 224)
    (make-flail area :x 592 :y 0)
    (make-fire area :x 120 :y 48)
    (make-fire area :x 456 :y 48)
    (make-cauldron area :x 208 :y 144)
    (make-eye area :x 376 :y 224)
    (make-ghost area :x 208 :y 0)
    (make-door-green area :x 240 :y 208 :key 'amphora)
    (make-door-left area :x 0 :y 192 :id :n1o1  :target
    	       :n1)
    (make-door-right area :x 592 :y 208 :id :o1p1 
    		:target :p1)
    (make-river area :x 304 :y 240 :size 9)
    )

  (make-area (area :O3
		   :background 44
		   :foreground 45
		   :stencil 24
		   :name "nearTheChateau")
	     (make-fleurdelys area :x 352 :y 224)
	     (make-goldkey area :x 488 :y 224)
	     (make-warlock area :x 256 :y 224)
	     (make-wildboar area :x 256 :y 128)
	     (make-door-left area :x 0 :y 208 :id :n3o3 
	     		:target :n3)
	     (make-door-right area :x 352 :y 0 :id :o3p4 
	     		 :target :p4)
	     (make-door-right area :x 560 :y 224 :id :o3q2
			      :key 'fleurdelys :target :q2)
	     (make-river area :x 0 :y 240 :size 4)
	     )

  (make-area (area :O5
		   :background 46
		   :foreground 47
		   :stencil 25
		   :name "palace")
	     (make-goblet area :x 372 :y 32)
	     (make-shootingstar area :x 416 :y 96)
	     (make-warlock area :x 336 :y 224)
	     (make-ghost area :x 64 :y 160)
	     (make-door-green area :x 416 :y 32 :key 'amphora)
	     (make-door-left area :x 0 :y 0 :id :n5o5
			     :target :n5)
	     (make-door-right area :x 592 :y 32 :id :o5p5 
	     		 :target :p5)
	     )

  (make-area (area :O6
		   :background 48
		   :foreground 49
		   :stencil 26
		   :name "palace")
	     (make-axe area :x 112 :y 224)
	     (make-shootingstar area :x 400 :y 96)
	     (make-wildboar area :x 208 :y 224)
	     (make-ghost area :x 352 :y 224)
	     (make-door-left area :x 0 :y 0 :id :n6o6
			     :target :n6)
	     (make-door-right area :x 592 :y 48 :id :o6p6 
	     		 :target :p6))

  (make-area (area :P1
		   :background 50
		   :stencil 27
		   :name "tunnel")
	     (make-goldkey area :x 328 :y 224)
	     (make-warlock area :x 240 :y 224)
	     (make-warlock area :x 480 :y 224)
	     (make-flail area :x 576 :y 224)
	     (make-fire area :x 264 :y 48)
	     (make-door-trap area :x 576 :y 160 :key 'goldkey)
	     (make-door-left area :x 0 :y 0 :id :o3q2
			     :target :o3)
	     (make-door-left area :x 0 :y 208 :id :o1p1 
	     		:target :o1)
	     (make-door-right area :x 592 :y 0 :id :p1q2 
	     		 :target :q2)
	     )

  (make-area (area :P4
		   :background 51
		   :foreground 52
		   :stencil 28
		   :name "aboveChateau")
	     (make-amphora area :x 256 :y 192)
	     (make-flail area :x 0 :y 144)
	     (make-warlock area :x 376 :y 192)
	     (make-eye area :x 376 :y 32)
	     (make-door-left area :x 0 :y 16 :id :p4p5
			     :target :p5)
	     (make-door-left area :x 0 :y 208 :id :o3p4 
	     		:target :o3)
	     (make-door-right area :x 592 :y 0 :id :p4q4 
	     		 :target :q4))

  (make-area (area :P5
		   :background 53
		   :foreground 54
		   :stencil 29
		   :name "palace")
	     (make-moon area :x 576 :y 32)
	     (make-flail area :x 160 :y 224)
	     (make-eye area :x 408 :y 192)
	     (make-warlock area :x 256 :y 224)
	     (make-cauldron area :x 32 :y 224)
	     (make-door-trap area :x 400 :y 96 :key 'goldkey)
	     (make-door-left area :x 0 :y 32 :id :o5p5
			     :target :o5)
	     (make-door-right area :x 512 :y 0 :id :p5p6 
	     		 :target :p6)
	     (make-door-right area :x 592 :y 208 :id :p4p5 
	     		 :target :p4)
	     )

  (make-area (area :P6
		   :background 55
		   :foreground 56
		   :stencil 30
		   :name "palace")
	     (make-magicwand area :x 192 :y 224)
	     (make-friend area :x 560 :y 240)
	     (make-shootingstar area :x 448 :y 96)
	     (make-head area :x 64 :y 192)
	     (make-ghost area :x 344 :y 192)
 	     (make-door-trap area :x 560 :y 176 :key 'littlelyre)
	     (make-door-left area :x 0 :y 48 :id :o6p6
			     :target :o6)
	     (make-door-right area :x 496 :y 48 :id :p5p6 
	     		 :target :p5)
	     )

  (make-area (area :Q2
		   :background 57
		   :foreground 58
		   :stencil 31
		   :name "chateau")
	     (make-magicwand area :x 576 :y 224)
	     (make-goldkey area :x 88 :y 32)
	     (make-warlock area :x 280 :y 32)
	     (make-wildboar area :x 264 :y 160)
	     (make-door-trap area :x 224 :y 48 :key 'goldkey)
	     (make-door-green area :x 512 :y 208 :key 'goldkey)
	     (make-door-left area :x 0 :y 32 :id :o3q2  :target
	     		:o3)
	     (make-door-left area :x 0 :y 224 :id :p1q2 
	     		:target :p1)
	     (make-door-right area :x 592 :y 32 :id :q2s3 
	     		 :target :s3)
	     )

  (make-area (area :Q4
		   :background 59
		   :stencil 32
		   :name "aboveChateau")
	     (make-spellbook area :x 64 :y 192)
	     (make-shootingstar area :x 544 :y 48)
	     (make-ghost area :x 256 :y 96)
	     (make-warlock area :x 208 :y 160)
	     (make-friend area :x 336 :y 224)
	     (make-door-trap area :x 336 :y 176 :key 'spellbook)
	     (make-door-left area :x 0 :y 0 :id :p4q4  :target
	     		:p4)
	     (make-door-left area :x 288 :y 224 :id :q4s2
			     :key 'fleurdelys :target :s2)
	     (make-door-right area :x 592 :y 0 :id :q4r4 
	     		 :target :r4)
	     )

  (make-area (area :R4
		   :background 60
		   :stencil 33
		   :name "aboveChateau")
	     (make-amphora area :x 64 :y 192)
	     (make-sword area :x 416 :y 192)
	     (make-warlock area :x 240 :y 192)
	     (make-eye area :x 512 :y 32)
	     (make-door-left area :x 16 :y 0 :id :q4r4  :target
	     		:q4)
	     (make-door-right area :x 576 :y 208 :id :r4s3 
	     		 :target :s3)
	     )

  (make-area (area :S2
		   :background 61
		   :foreground 62
		   :stencil 34
		   :name "chateau")
	     (make-amphora area :x 576 :y 144)
	     (make-goldkey area :x 440 :y 64)
	     (make-warlock area :x 360 :y 64)
	     (make-warlock area :x 200 :y 224)
	     (make-door-trap area :x 288 :y 80 :key 'goldkey)
	     (make-door-green area :x 272 :y 224 :key 'goldenchalice)
	     (make-door-left area :x 0 :y 64 :id :q2s3
			     :target :q2)
	     (make-door-right area :x 592 :y 64 :id :q4s2 
	     		 :target :q4)
	     (make-door-right area :x 592 :y 224 :id :s2u2 
	     		 :target :u2)
	     )

  (make-area (area :S3
		   :background 63
		   :foreground 64
		   :stencil 35
		   :name "nearTheChateau")
	     (make-goldenchalice area :x 272 :y 176)
	     (make-axe area :x 384 :y 224)
	     (make-ghost area :x 512 :y 32)
	     (make-head area :x 336 :y 128)
	     (make-door-left area :x 176 :y 0 :id :r4s3  :target
	     		:r4)
	     (make-door-left area :x 32 :y 208 :id :q2s3
			     :key 'fleurdelys :target :q2)
	     (make-door-right area :x 592 :y 224 :id :s3t3 
	     		 :target :t3)
	     )

  (make-area (area :T3
		   :background 65
		   :stencil 36
		   :name "woods")
	     (make-fleurdelys area :x 528 :y 208)
	     (make-cauldron area :x 192 :y 112)
	     (make-shootingstar area :x 560 :y 52)
	     (make-head area :x 128 :y 48)
	     (make-eye area :x 512 :y 128)
	     (make-door-green area :x 320 :y 208 :key 'amphora)
	     (make-door-left area :x 0 :y 224 :id :s3t3 
	     		:target :s3)
	     (make-door-right area :x 592 :y 0 :id :t3u3 
	     		 :target :u3)
	     (make-river area :x 384 :y 240 :size 4)
	     )

  (make-area (area :U2
		   :background 66
		   :foreground 67
		   :stencil 37
		   :name "chateau")
	     (make-flail area :x 512 :y 224)
	     (make-head area :x 552 :y 128)
	     (make-ghost area :x 372 :y 128)
	     (make-friend area :x 128 :y 224)
	     (make-spellbag area :x 192 :y 144)
	     (make-cauldron area :x 352 :y 224)
	     (make-door-green area :x 224 :y 224 :key 'amphora)
	     (make-door-left area :x 0 :y 48 :id :q4s2  :target
	     		:s2)
	     (make-door-left area :x 0 :y 224 :id :s2u2 
	     		:target :s2)
	     (make-door-right area :x 592 :y 0 :id :u2v3 
	     		 :target :v3)
	     )
  (make-area (area :U3
		   :background 68
		   :stencil 38
		   :name "nearTheVillage")
	     (make-sword area :x 224 :y 224)
	     (make-amphora area :x 320 :y 224)
	     (make-warlock area :x 96 :y 224)
	     (make-head area :x 416 :y 8)
	     (make-door-left area :x 0 :y 0 :id :t3u3  :target
	     		:t3)
	     (make-door-right area :x 592 :y 0 :id :u3v3 
	     		 :target :v3)
	     )
  (make-area (area :V3
		   :background 69
		   :stencil 39
		   :name "nearTheVillage")
	     (make-shootingstar area :x 544 :y 32)
	     (make-flail area :x 496 :y 208)
	     (make-warlock area :x 372 :y 224)
	     (make-head area :x 256 :y 8)
	     (make-door-left area :x 0 :y 0 :id :u3v3  :target
	     		:u3)
	     (make-door-left area :x 32 :y 224 :id :u2v3 
	     		:target :u2)
	     ))
