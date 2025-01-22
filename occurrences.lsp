
(defun occurrences (lst)
  (occurrencesacc lst (list)))

(defun occurrencesacc (lst acc)
  (if (null lst)
      (sort acc #'> :key #'cdr)
      (if (member (car lst) acc :key #'car)
	  (occurrencesacc (cdr lst) acc)
	  (occurrencesacc (cdr lst) (push (cons (car lst) (count (car lst) lst)) acc)))))

