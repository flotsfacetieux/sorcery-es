(in-package :sorcery-es)

(defparameter *cheat-code* "aurmar")

(let ((index 0))

  (defun cheat-code-reset ()
    (setf index 0)
    (setf *cheat-mode* nil))
  (defun cheat-code-key (char)
    (when (not *cheat-mode*)
      (if (eq char (aref *cheat-code* index))
	  (incf index)
	  (cheat-code-reset))
      (when (= index (length *cheat-code*))
	(format t "cheat code actif~%")
	(setf *cheat-mode* t)))))
