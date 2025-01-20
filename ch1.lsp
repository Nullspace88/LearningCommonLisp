
(defun embedded-listp (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
	  T
	  (embedded-list (cdr lst)))))

(defun dotprinterr (num)
  (if (< num 1)
      nil
      (progn
	(format t ".")
	(dotprinterr (- num 1)))))

(defun dotprinteri (num)
  (do ((i num (- i 1)))
      ((< i 1) nil)
    (format t ".")))

(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit2 (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
	(if (null x)
	    (summit2 (cdr lst))
	    (+ x (summit2 (cdr lst)))))))
