
(defun lensort (a b)
  (< (length a) (length b)))

(defvar sortstr ())

(let ((strlist ()))
  (defun push-if-not-nil (x)
    (if (null x) x (push x strlist)))
  (defun write-if-not-nil (x)
    (if (null x) x (format t x)))
  (defun pop-and-maybe-write ()
    (write-if-not-nil (pop strlist)))
  (defun get-sorted-list ()
    (sort strlist #'lensort))
  )



(defun main () (progn
		 (loop for i upto 5 while (push-if-not-nil (read-line t nil nil)))
		 (let ((sortstr (get-sorted-list)))
		   (loop while (pop-and-maybe-write)))))
