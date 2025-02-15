
(defun maxandmin (lst)
  (maxandminacc lst (car lst) (car lst)))

(defun maxandminacc (lst max min)
  (if (null lst)
      (values max min)
      (progn
	(cond ((and (>= (car lst) max) (<= (car lst) min))
	       (maxandminacc (cdr lst) (car lst) (car lst)))
	      ((and (>= (car lst) max) (>= (car lst) min))
	       (maxandminacc (cdr lst) (car lst) min))
	      ((and (<= (car lst) max) (>= (car lst) min))
	       (maxandminacc (cdr lst) max min))
	      ((and (<= (car lst) max) (<= (car lst) min))
	       (maxandminacc (cdr lst) max (car lst)))
	      ((and (null max) (null min)
		    (maxandminacc (cdr lst) (car lst) (car lst))))
	      (t (maxandminacc (cdr lst) max min))))))
	    
