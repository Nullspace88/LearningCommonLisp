
(defun new-union (lst1 lst2)
  (new-unionacc lst1 lst2 (list)))


(defun new-unionacc (lst1 lst2 acc)
  (if (null lst1)
      (reverse acc)
      (if (member (car lst1) lst2 :test #'equal)
	  (new-unionacc (cdr lst1) lst2 (push (car lst1) acc))
	  (new-unionacc (cdr lst1) lst2 acc))))
