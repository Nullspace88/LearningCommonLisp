
(defun pos+r (lst)
  (pos+racc lst 0 (list)))

(defun pos+racc (lst inx acc)
  (if (null lst)
      acc
      (pos+racc (cdr lst) (+ inx 1) (append acc (list (+ (car lst) inx))))))
       
(defun pos+mc (lst)
  (mapcar #'+ lst (genlist (length lst))))

(defun genlist (n)
  (genlistacc n 0 (list)))

(defun genlistacc (n inx acc)
  (if (< n 1)
      acc
      (genlistacc (- n 1) (+ inx 1) (append acc (list inx)))))

(defun pos+i (lst)
  (pos+iacc lst (list)))

(defun pos+iacc (lst acc)
   (do ((i 0 (+ i 1)))
       ((> i (- (length lst) 1)) acc)
     (setf acc (append acc (list (+ (nth i lst) i))))))
