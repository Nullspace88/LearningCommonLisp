(setf path (make-pathname :name "myfile"))

(setf str (open path :direction :output
                     :if-exists :supersede))

(format str "Something~%")
(close str)

