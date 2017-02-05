(in-package :sorcery-es)

;;
;; stencils
;; 

(defun load-stencils ()
  ;; load stencils files
  (with-open-file (s *areas-stencils-pathname*
		     :direction :input
		     :element-type '(unsigned-byte 8))
    (read-sequence *areas-stencils* s))
  (with-open-file (s *sprites-stencils-pathname*
		     :direction :input
		     :element-type '(unsigned-byte 8))
    (read-sequence *sprites-stencils* s)))

;; Return the bit of the stencil array
;; at position x y for cell of dimension w h.
;; Stencil are one dimension array.
(defun stencil-read-bit (seq cell x y w h)
  (sbit seq (+ x (* y w) (* cell w h))))

;; For debug
(defun display-stencil-data (seq cell w h)
  (dotimes (y h)
    (dotimes (x w)
      (format t "~a" (stencil-read-bit seq cell x y w h)))
    (format t "~%")))
