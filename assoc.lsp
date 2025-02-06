
(defun assoc-to-hash (assoc)
  (let ((ht (make-hash-table)))
    (dolist (x assoc)
      (setf (gethash (car x) ht) (cdr x)))
    ht))
    
(defun hash-to-assoc (ht)
  (let ((alist nil))
    (maphash #'(lambda (k v)
		 (setf alist (cons (cons k v) alist)))
	     ht)
    alist))
