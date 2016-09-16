(defun star-if-on-cross (x y max)
  (cond
   ((= x y) "*")
   ((= x (- (1- max) y)) "*")
   (t " ")))

(defun make-cross (max)
  (loop for y from 0 to (1- max) do
       (format t "~%")
       (loop for x from 0 to (1- max) do
            (format t (star-if-on-cross x y max)))))

(make-cross 9)
