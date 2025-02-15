
(defun interspersei (x lst)
  (let ((acc nil))
    (dotimes (y (length lst) (reverse acc))
      (if (= y 0)
	  (setf acc (cons (elt lst y) acc))
	  (setf acc (cons (elt lst y) (cons x acc)))))))

(defun intersperser (x lst)
  (setf acc nil)
  (intersperseracc x lst)
  (reverse acc))


(defun intersperseracc (x lst)
  (if (null lst)
      nil
      (progn
	(if (null acc)
	    (setf acc (cons (car lst) acc))
	    (setf acc (cons (car lst) (cons x acc))))
	(intersperseracc x (cdr lst)))))
