
(defun precedes (o v) (precedeswrap o v ()))
(defun precedeswrap (o v acc)
  (cond ((>= 1 (length v)) acc)
        ((eql o (aref v 1)) (precedeswrap o (subseq v 1 (length v)) (cons (aref v 0) acc)))
        (t (precedeswrap o (subseq v 1 (length v)) acc))))

(defun precedesitr (o v) (pitrwrap o v ()))
(defun pitrwrap (o v acc)
  (do ((i 0 (+ i 1)))
      ((>= i (- (length v) 1)) acc)
      (if (eql o (aref v (+ i 1))) (setf acc (cons (aref v i) acc)))))
