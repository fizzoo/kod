(defun real-number (nr base)
  (let ((list nil)
        (sum 0))
    (loop until (eq nr 0)
       do (progn
            (setq list (cons (mod nr 10) list))
            (setq nr (floor nr 10))))
    (loop for i in list do
         (setq sum (+ (* sum base) i)))
    sum))

(real-number 47 29)


(loop for i from 1 to 1000 do
     (let ((nr (+ 100 (random 10000)))
           (base (+ 20 (random 40))))
       (format t "test ~a ~a ~a~%" (real-number nr base) nr base)))
