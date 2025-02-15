
(defun squareif (x)
  (cond ((and (> x 0) (<= x 5)) (values))
	(t (* x x))))
