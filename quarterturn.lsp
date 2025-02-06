
(setf test #2A((a b) (c d)))
(setf test2 #2A((1 2 3) (4 5 6) (7 8 9)))

(defun quarter-turn (array)
  (let ((dim (array-dimensions array)))
    (if (not (eql 2 (length dim)))
	'nottwo
	(if (not (eql (nth 0 dim) (nth 1 dim)))
	    'notsquare
	    (let ((new (make-array dim)))
	      (do ((n (- (nth 1 dim) 1) (- n 1)))
		  ((< n 0) new)
		(do ((n2 (- (nth 0 dim) 1) (- n2 1)))
		    ((< n2 0) 'rowdone)
		  (setf (aref new n2 (- (- (nth 0 dim) 1) n)) (aref array n n2)))))))))
