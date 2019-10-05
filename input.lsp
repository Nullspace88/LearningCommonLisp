(setf path (make-pathname :name "newmyfile"))

(with-open-file (str path :direction :output
                          :if-exists :supersede)
    (format str "Something~%"))
