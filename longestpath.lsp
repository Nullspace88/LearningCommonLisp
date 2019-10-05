(defun longest-path (start end net)
  (get-longest (solution-paths start end net)))

(defun solution-paths (start end net)
  (let ((slns ()))
    (bfs end (list (list start)) net slns)))

(defun get-longest (paths)
  (if (null paths)
      nil
      (let ((lpaths (pnl paths)))
        (car (car (sort lpaths #'(lambda (x y) (> (cdr x) (cdr y))))))))) 

(defun pnl (paths)
  (mapcar #'(lambda (n) (cons n (list-length n))) paths))

(defun bfs (end queue net slns)
  (if (null queue)
    slns 
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
            (progn
              (push (reverse path) slns)
              (bfs end (cdr queue) net slns))
            (bfs end
                 (append (cdr queue)
                         (new-paths path node net))
                 net slns))))))
  
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (if (not (member n path))
                  (cons n path)))
          (cdr (assoc node net))))
