(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
            (reverse path)
            (bfs end
                 (append (cdr queue)
                         (new-paths path node net))
                 net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(defun new-union (ls1 ls2))


(defun our-new-union (ls1 ls2)
  (dolist (elm (reverse ls1))
    (setf ls2 (adjoin elm ls2)))
  ls2)

(defun occurrences (lst)
  (if (consp lst)
      (sort (occur lst ()) #'(lambda (x y) (> (cdr x) (cdr y))))
      lst))

(defun occur (lst out)
  (if (null lst)
      out
      (occur (remove (car lst) lst) (cons (cons (car lst) 
                                                (count (car lst) lst))
                                          out)))) 
;;(defun pos+ (lst)
;;  (if (consp lst)
;;      (reverse (rpos lst () 0))
;;      lst))
;;
;;(defun rpos (lst out n)
;;  (if (null lst)
;;      out
;;      (rpos (cdr lst) (cons (+ (car lst) n) out) (+ n 1))))

;;(defun pos+ (lst)
;;  (if (consp lst)
;;      (reverse (rit lst ()))
;;      lst))
;;
;;(defun rit (lst out)
;;  (do ((i 0 (+ i 1)))
;;      ((>= i (list-length lst)) out)
;;    (setf out (cons (+ (nth i lst) i) out))))

(defun pos+ (lst)
  (mapcar #'+ lst (toN (list-length lst))))

(defun toN (n)
  (if (<= n 0)
      nil 
      (wrapn n ())))

(defun wrapn (n lst)
  (if (< n 0)
      lst
      (wrapn (- n 1) (cons n lst))))

(defun showdots (lst)
  (if (null lst)
      nil
      (outdots lst (list-length lst))))

(defun outdots (lst startlen)
  (if (null lst)
      (progn
        (format t "NIL")
        (format t "~v@{~A~:*~}" startlen ")"))
      (progn
        (format t "(~A . " (car lst))
        (outdots (cdr lst) startlen))))
