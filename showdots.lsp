
(defun showdots (lst)
  (showdotsn lst 0))

(defun showdotsn (lst n)
  (if (null lst)
      (progn
	(format t "NIL")
	(do ((i 0 (+ i 1)))
	    ((> i (- n 1)) nil)
	  (format t ")")))
      (progn
	(format t "(~A . " (car lst))
	(showdotsn (cdr lst) (+ n 1)))))
