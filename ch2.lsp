;; rotate n x n array 90 degrees
(load "binsearchtree.lsp")

(defun rotate (array)
  (if (null array)
      array
      (let ((final (make-array (array-dimensions array)
                              :INITIAL-ELEMENT nil)))
        (do ((i 0 (+ i 1)))
            ((>= i (array-dimension array 0)) final)
          (do ((j 0 (+ j 1)))
              ((>= j (array-dimension array 1)))
            (setf (aref final i j) (aref array (-(array-dimension array 1) (+ j 1)) i)))))))

(defun copy-list2 (list) 
  (reduce #'cons list :initial-value nil :from-end t)) 

(defun revcon (lst elm)
  (cons elm lst))

(defun reverse-lst (list)
  (reduce #'revcon list :initial-value nil))

(defstruct ttree
  node
  left
  center
  right)

(defun copytt (ttr) 
  (if (null ttr)
      ttr
      (make-ttree :node   (ttree-node ttr)
                  :left   (copytt (ttree-left ttr))
                  :right  (copytt (ttree-right ttr))
                  :center (copytt (ttree-center ttr)))))

(defun findtt (obj ttr)
  (if (null ttr)
      ttr
      (if (eq (ttree-node ttr) obj)
          T
          (or (findtt obj (ttree-left ttr))
              (findtt obj (ttree-center ttr))
              (findtt obj (ttree-right ttr))))))

(defun orderedr (bst out)
  (when bst
       (orderedr (node-r bst) out)
       (setf out (cons out (node-elt bst)))
       (orderedr (node-l bst) out)))

(defun dec-bst (bsti)
  (let ((out ()))
    (progn
      (defun bst-t (bst)
       (when bst
         (bst-t (node-l bst))
         (setf out (cons (node-elt bst) out))
         (bst-t (node-r bst))))
      (bst-t bsti)
      out)))
     
(defun assocToMap (alist)
  (let ((hmap (make-hash-table)))
    (progn
      (defun addToMap (list)
        (when list
          (setf (gethash (car (car list)) hmap) (cdr (car list)))
          (addToMap (cdr list))))
      (addToMap alist)
      hmap)))
          
(defun mapToAssoc (map)
  (let ((alist ()))
    (progn
      (defun addToList (k v)
        (setf alist (cons (cons k v) alist)))
      (maphash #'addToList map)
      alist)))
