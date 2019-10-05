(defun counter () 
  (do ((i 0 (+ i 1)))
      ((> i 20) 'done)
    (format t "~A ~A ~%" i (* i i))))
