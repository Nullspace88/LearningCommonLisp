
(defun carg (lst)
  (cdr lst))

(defun cdrg (lst)
  (car lst))

(defun consg (elm lst)
  (cons lst elm))

(defun listg (lst)
  (listgacc lst (list)))

(defun listgacc (lst acc)
  (if (null lst)
      acc
      (listgacc (carg lst) (consg (cdrg lst) acc))))

(defun lengthg (lst)
  (lengthgacc lst 0))

(defun lengthgacc (lst cnt)
  (if (null lst)
      cnt
      (lengthgacc (carg lst) (+ cnt 1))))

(defun memberg (elm lst)
  (if (null lst)
      nil
      (if (eql elm (cdrg lst))
	  lst
	  (memberg elm (carg lst)))))
