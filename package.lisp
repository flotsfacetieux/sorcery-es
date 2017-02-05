(in-package #:cl-user)

(defpackage :sorcery-es
  (:nicknames :se)
  (:use #:cl :alexandria #:cl-es)
  (:export #:start
	   #:load-graphics
	   #:*base-directory*
	   #:*zoom-scale*
	   #:*sprites-stencils-pathname*
	   #:*sprites-sheet*
	   #:*areas-stencils-pathname*
	   #:*areas-sheet*
	   #:sheet))

(defparameter sorcery-es:*base-directory* 
  (make-pathname :name nil :type nil 
		 :defaults (asdf:system-source-directory :sorcery-es)))

(defpackage #:sorcery-es-stencils-builder
  (:nicknames :se-sb)
  (:use #:cl)
  (:import-from :se
		#:load-graphics
		#:*zoom-scale*
		#:*sprites-stencils-pathname*
		#:*sprites-sheet*
		#:*areas-stencils-pathname*
		#:*areas-sheet*
		#:sheet)
  (:export #:build-stencils-file))
