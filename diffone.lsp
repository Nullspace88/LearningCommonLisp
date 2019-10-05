
(defun diffone (l)
  (cond ((<= (length l) 1) t)
        ((propdiff l) (diffone (cdr l)))
        (t nil)))

(defun propdiff (l)
  (cond ((eql (abs(- (car l) (car (cdr l)))) 1) t)
        (t nil)))
      
(defun itrdiff (l)
  (do ((i 0 (+ i 1)))
       ((>= i (- (length l) 1)) t)
    (cond ((eql (abs(- (nth i l) (nth (+ i 1) l))) 1))
          (t nil))))

(defun mapdiff (l)
  (mapc #'(lambda (x y)
  (cond ((not (eql (abs (- x y)) 1)) (return-from mapdiff nil))))
    l (cdr l)))

