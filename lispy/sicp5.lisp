(defun make-accumulator (balance)
  (lambda (addition)
    (setf balance (+ balance addition))
    balance))

(defvar myacc (make-accumulator 5))

(funcall myacc 10)

(defun make-monitored (fun)
  (let ((counter 0))
    (lambda (funpar)
      (if (eq funpar 'how-many-calls?)
          counter
          (progn
            (setf counter (1+ counter))
            (funcall fun funpar))))))

(defvar s (make-monitored #'sqrt))

(funcall s 100)
(funcall s 'how-many-calls?)

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0)
                    (/ trials-passed trials))
                   ((funcall experiment)
                    (iter (- trials-remaining 1) (+ trials-passed 1)))
                   (t
                    (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))

(defun cesaro-test ()
  (= (gcd (random 1000000000) (random 1000000000)) 1))
(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(estimate-pi 10000000)

(let ((x (cons 1 2)))
  (setf (cdr x) 3)
  x)

(let ((x (list 1 2)))
  (setf (cdr x) 3)
  x)

(let ((a (list 1 2))
      (b (list 3 4)))
  (list
   a
   b
   (nconc a b)
   a
   b))

(defun count-pairs (x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(defun number-to-fizz (n)
  (cond ((= 0 (mod n 15)) "fizzbuzz")
        ((= 0 (mod n 3)) "fizz")
        ((= 0 (mod n 5)) "buzz")
        (t n)))

(defun fizz-buzz ()
  (loop for i from 1 to 100
     do (print (number-to-fizz i))))

(fizz-buzz)

(defun buzz (n)
  (if (stringp n)
      "fizzbuzz"
      "buzz"))

(let ((list (loop for i from 1 to 100 collecting i)))
  (do ((f 2 (+ f 3)))
      ((>= f 100) t)
    (setf (elt list f) "fizz"))
  (do ((f 4 (+ f 5)))
      ((>= f 100) t)
    (setf (elt list f) (buzz (elt list f))))
  (mapc #'print list))

(let ((list (loop for i from 1 to 100 collecting i)))
  (loop for i from 2 to 100 by 3 do
       (setf (elt list i) "fizz"))
  (loop for i from 4 to 100 by 5 do
       (setf (elt list i) (buzz (elt list i))))
  (mapc #'print list))

(mapcar (lambda (x) (* x 2)) (loop for i from 3 to 100 by 3 collecting i))

(defun sum (x)
  (apply #'+ x))

(sum '(1 2 3))


(defun my-cons (x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (t (error "Argument not 0 or 1 -- CONS")))))

(my-cons 1 2)

(defun my-car (z) (funcall z 0))
(defun my-cdr (z) (funcall z 1))

(my-car (my-cons 1 2))


(defun triple? (x)
  (= 0 (mod x 3)))
(defun range (x)
  (loop for i from 0 to (1- x) collecting i))
(remove-if-not #'triple? (range 10))

(defun atom? (x)
  (atom x))
(defun lat? (list)
  (or (not list)
      (and (atom? (car list))
           (lat? (cdr list)))))

(lat? '(1 2 3))
(lat? '((1) 2 3))
