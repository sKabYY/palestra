(setf a 'global-a)
(defvar *b* 'global-b)

(defun fn () *b*)

(print
 (let ((a 'local-a) (*b* 'local-b))
   (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*))))