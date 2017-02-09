(defsystem "sorcery-es"
    :name "Sorcery Entity System"
    :version "0.0.1"
    :maintainer "Flot Facetieux"
    :author "Flot Facetieux"
    :licence "MIT"
    :serial t
    :description "Sorcery refactored with Entity System Components."
    :long-description
    ""
    :depends-on (:alexandria
		 :lispbuilder-sdl-image
		 :lispbuilder-sdl-gfx
		 :lispbuilder-sdl-ttf
		 :swank
		 :cl-entity-system)
    :components ((:file "package")
		 (:file "conf")
		 (:file "state")
		 (:file "board")
		 (:file "cheat-code")
		 (:file "components")
		 (:file "overlaps")
		 (:file "text")
		 (:file "elements")
		 (:file "data")
		 (:file "sprites-sheet")
		 (:file "stencil")
		 (:file "stencils-builder")
		 (:file "action")
		 (:file "move")
		 (:file "summary")
		 (:file "outside")
		 (:file "costume")
		 (:file "contact")
		 (:file "collide")
		 (:file "countdown")
		 (:file "player")
		 (:file "roll-text")
		 (:file "render")
		 (:file "game")
		 (:file "victory")
		 (:file "intro")
		 (:file "sorcery")))

    
