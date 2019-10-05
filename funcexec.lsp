(defun length (alist) 
    (if (null alist)
        0
        (+ (length (cdr alist)) 1)))

(defun numargs (&rest args)
    (length(args)))
