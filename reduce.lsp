
(defun our-copy-list (lst)
  (let ((x (reduce #'list lst :from-end t :initial-value nil)))))

(defun our-reverse (lst)
  (reduce #'(lambda (x y) (list y x)) lst))
