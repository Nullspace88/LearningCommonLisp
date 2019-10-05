(defun ex ()
  (block test
    (block head
        (format t "Here we go.")
        (return-from head 'headexit)
        (format t "We'll never see this."))
    (return-from test 'testexit)))
  

