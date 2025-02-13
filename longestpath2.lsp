
(setf min '((a b c) (b e) (c d)))

(defun longest-path (start end net)
  (dfs end (list (list start)) net))

(defun dfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (equal 2 (count node path))
	      (dfs end (cdr queue) net)
	      (if (eql node end)
		  (reverse path)
		  (dfs end
		       (append (cdr queue)
			       (new-paths end queue path node net))
		       net)))))))

(defun new-path (end queue path node net)
  (if (null (assoc node net))
      (new-path end queue path (car (cdr path)) net)
  (let ((newnode (pop (cdr (assoc node net)))))
    (progn
      (format t "node ~A newnode:~A path: ~A queue:~A" node newnode (assoc node net) queue))
      (read)
      (if (null newnode)
 	(dfs end (cdr queue) net)
      (if (member newnode path :test #'equal)
	  (new-path end queue path node net)
	  (list (cons newnode path)))))))

(defun new-paths (end queue path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))
