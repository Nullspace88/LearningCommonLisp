
(defstruct (ourtree (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (ourtree-elt n)))))
  elt (l nil) (c nil) (r nil))

(defun ourtree-copy (tree)
  (if (null tree)
      nil
      (make-ourtree
       :elt (ourtree-elt tree)
       :l (ourtree-copy (ourtree-l tree))
       :c (ourtree-copy (ourtree-c tree))
       :r (ourtree-copy (ourtree-r tree)))))

(defun ourtree-insert (obj tree <)
  (if (null tree)
      (make-ourtree :elt obj)
      (let ((elt (ourtree-elt tree)))
	(if (eql obj elt)
	    (make-ourtree
	     :elt elt
	     :l (ourtree-l tree)
	     :c (ourtree-c tree)
	     :r (ourtree-r tree))
	    (let ((x (random 3)))
	      (if (eql x 0)
		  (make-ourtree
		   :elt elt
		   :l (ourtree-insert obj (ourtree-l tree) <)
		   :c (ourtree-c tree)
		   :r (ourtree-r tree))
		  (if (eql x 1)
		      (make-ourtree
		       :elt elt
		       :l (ourtree-l tree)
		       :c (ourtree-insert obj (ourtree-c tree) <)
		       :r (ourtree-r tree))
		      (if (eql x 2)
			  (make-ourtree
			   :elt elt
			   :l (ourtree-l tree)
			   :c (ourtree-c tree)
			   :r (ourtree-insert obj (ourtree-r tree) <))))))))))
   

(defun ourtree-traverse (fn tree)
  (when tree
    (ourtree-traverse fn (ourtree-l tree))
    (ourtree-traverse fn (ourtree-c tree))
    (funcall fn (ourtree-elt tree))
    (ourtree-traverse fn (ourtree-r tree))))

(defun ourtree-find (obj tree)
  (if (null tree)
      nil
      (or (ourtree-find obj (ourtree-l tree))
	  (ourtree-find obj (ourtree-c tree))
	  (eql (ourtree-elt tree) obj)
	  (ourtree-find obj (ourtree-r tree)))))

		 
