(defun cube (a) (* a a a))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))

(defun inc (a) (+ a 1))
(defun sum-cubes (a b)
  (sum #'cube a #'inc b))

(sum-cubes 1 10)

(defun id (x) x)

(id 1)

(print 1)

(format t "heja")

(let ((counter -5))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(new-id)
;; (print counter)                         ;unbound
(reset-id)

(1+ 1)

(let ((lst ()))
  (push 1 lst)
  (pop lst))

(defun rev (lst)
  (let ((rlst ()))
    (labels ((pushall (x)
               (if (null x)
                   nil
                   (progn
                     (push (car x) rlst)
                     (pushall (cdr x))))
               ))
      (pushall lst)
      rlst)))

(rev '(1 2 3))

(defun sum-integers (a b)
  (sum #'id a #'inc b))

(sum-integers 1 10)

(loop
   for i upto 10
   collect (loop
              for j upto 10
              collecting (cons i j)) into cij
   finally (return (apply #'append cij)))

(defun pi-sum (a b)
  (defun pi-term (x)
    (/ 1.0 (* x (+ x 2))))
  (defun pi-next (x)
    (+ x 4))
  (sum-acc 0 #'pi-term a #'pi-next b))

(defun sum-acc (acc term a next b)
  (if (> a b)
      acc
      (sum-acc (+ acc (funcall term a)) term (funcall next a) next b)))


;;; löste så att den TCO-ade genom ny sum
;;; verkar fixa det finfint

(* 8 (pi-sum 1 100000000))

(defun integral (f a b dx)
  (defun add-dx (x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) #'add-dx b)
     dx))

(integral #'cube 0 1 0.01)
(integral #'cube 0 1 0.001)

(integral (lambda (x) (1+ x))
          0 1 0.001)

(defun average (x y)
  (/ (+ x y) 2))

(defun close-enough? (x y)
  (< (abs (- x y)) 0.00001))

(defun searchf (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (funcall f midpoint)))
          (cond ((> test-value 0)
                 (searchf f neg-point midpoint))
                ((< test-value 0)
                 (searchf f midpoint pos-point))
                (t midpoint))))))

(searchf (lambda (x) x)
         -1
         2)

(defun negative (x)
  (< x 0))
(defun positive (x)
  (> x 0))

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))
    (cond ((and (negative a-value) (positive b-value))
           (searchf f a b))
          ((and (negative b-value) (positive a-value))
           (searchf f b a))
          (t (error "bad val")))))

(half-interval-method #'sin 2.0 4.0)


(defvar *tolerance* 0.00001)
(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) *tolerance*))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point #'cos 1.0)


(defun is-function (x)
  (equal (type-of x) 'FUNCTION))

(is-function 2)

(type-of '(1 2 'hej))
(type-of '(1 . 2))

(defun neq (x y)
  (not (equal x y)))
(neq 'case 'cond)

(defmacro make-one-if-not-list (lst)
  (if (neq (type-of lst) 'CONS)
      1
      lst))

(make-one-if-not-list '(1 2 3))
(make-one-if-not-list 2)

(defun is-list (x)
  (equal (type-of x) 'CONS))

(defmacro make-one-if-not-listv2 (lst)
  (if (is-list lst)
      lst
      1))

(make-one-if-not-list '(1 2 3))
(make-one-if-not-list 2)
(null nil)
(null '())

(defun make-one-recurse (lst)
  (cond
    ((null lst) nil)
    ((atom lst) 1)
    (t (cons
        (make-one-recurse (car lst))
        (make-one-recurse (cdr lst))))))

(defun loud-treecurse (lst)
  (cond
    ((null lst) (print "null"))
    ((atom lst) (print "atom"))
    (t (print "<")
       (loud-treecurse (car lst))
       (print ">")
       (loud-treecurse (cdr lst))
       (print "----"))))

(format t "~%")
'('())
(loud-treecurse '(1))
(loud-treecurse '('1))
(loud-treecurse '(1 2 3))
(loud-treecurse '(1 '() 3))
(loud-treecurse '('()))
(make-one-recurse '(1 2 3))
(atom '())
(atom '(1))
(atom nil)
(cdr '())
(make-one-recurse '(1 '() 3))
(make-one-recurse '('(1)))
(make-one-recurse '(1 2 3 '(1 2)))
(type-of '(1 2 3 '(1 2)))
(equal (type-of '(1)) 'CONS)
(atom nil)
(atom 1)
(atom '())
(atom '(1))
(cons 1 nil)
(cons '() '())

(defun rec-type-of (x)
  (if (is-list x)
      (mapcar #'rec-type-of x)
      (type-of x)))

(rec-type-of 1)

(defun replace-funcall (isfirst x flist)
  (if (atom x)
      x
      (append (cond ((not (atom (car x))) `(,(replace-funcall t (car x) flist)))
                    ((and isfirst (member (car x) flist)) `(funcall ,(car x)))
                    (t `(,(car x))))
              (replace-funcall nil (cdr x) flist))))


(atom '(print "hej"))
(defvar id (lambda (x) x))
(replace-funcall t 'x '(id))
(replace-funcall t '(x) '(id))
(replace-funcall t '(1 2) '(id))
(replace-funcall t '(id 2 3) '(id))
(replace-funcall t '('(+ (id 2) 3)) '(id))
(replace-funcall t '('(+ (f1 2) (f2 3))) '(f1 f2))
(replace-funcall t '('(+ (f1 y) (f y))) '(f1 y))

(defmacro print-body (&body body)
  (mapcar #'print body)
  nil)

(print-body 1)
(print-body 1 2 3)
(print-body
 (+ 1 (/ 1 2))
 (- 1 2))

;;; Scheme define. Note that a variable in car-position on a list is
;;; assumed to be a function, which does not 100% correspond with
;;; usage of cond and such macros.
(defmacro define (namelist &body body)
  `(defun ,(car namelist) ,(cdr namelist) ,@(mapcar
                                             (lambda (x)
                                               (replace-funcall t x (cdr
                                                                     namelist)))
                                             body)))

;;; konvergerar ej
;; (define (sqrtf x)
;;   (fixed-point (lambda (y) (/ x y))
;;             1.0))

;;; konvergerar
(define (sqrtf x)
    (fixed-point
     (lambda (y) (average y (/ x y)))
     1.0))

(define (add-del f x)
    (- (f x) (f (- x))))

;;; Does not work due to b assumed to be a function
;; (define (print-if-true b)
;;   (cond (b (print "true..."))
;;      (t nil)))

(intern "aaa")
(print #C(1 2))
(/ 1 3)
(not 0)
(not (not 0))
(elt "Apple" 0)

(let ((a 0))
  (defun next-id ()
    (setf a (1+ a))
    a)
  (defun reset-id ()
    (setf a 0)))

(next-id)
(reset-id)

(let ((a 1))
  (defun next-two ()
    (setf a (* a 2))
    a)
  (defun reset-two ()
    (setf a 1)))

(next-two)
(reset-two)

(defstruct dog name breed age)
(defparameter *rover*
  (make-dog :name "rover"
            :breed "collie"
            :age 5))

*rover*
(dog-p *rover*)
(dog-breed *rover*)
(setf (dog-age *rover*) (1+ (dog-age *rover*)))
(dog-age *rover*)

(cons 1 (cons 2 (cons 3 nil)))

(concatenate 'list '(1 2) '(3 4))
(concatenate 'string "hej" "kej")

(defparameter *sillylist* (cons 1 2))

;; fungerar, men dumt... inf
;; (rplacd *sillylist* *sillylist*)

(every #'numberp '(1 2 3))
(every #'numberp '(1 2 (3)))
(some #'numberp '(1 2 (3)))
(butlast '(1 2 3))
(butlast '(1 2 (3)))
(remove-if-not #'evenp '(1 2 3 4))

#(1 2 3)

(concatenate 'vector #(1 2 3) #(4 5 6))
;; not type list
;; (append #(1 2 3) #(4 5 6))

;;; 3-d array
(defparameter *arry* (make-array '(3 3 3)))

(aref *arry* 0 0 0)
(aref *arry* 2 2 2)
;; (aref *arry* 3 2 2)

(intersection '(3 2 1) '(2 3 4))
(union '(2 1 3 4) '(4 5 6 7 2))

;;; hash-table / dictionary
(defparameter *m* (make-hash-table))
(setf (gethash 'a *m*) 1)
(gethash 'a *m*)
(gethash 'b *m*)
(gethash 'b *m* 'not-found)
(setf (gethash 1 *m*) 2)
(gethash 1 *m*)

(multiple-value-bind (a b)
    (gethash 'a *m*)
  (list a b))

(describe :bar)
(describe keyword::bar)
(describe #'setf)

(let ((a '('a 'b 'c 'a 'nejnej)))
  (print a))

(let ((a (list 'a 'b 'c 'a 'nejnej)))
  (setf (cdr a) 8))

(apply (lambda () "Hello World") nil)

(defun hello (name)
  (format nil "Hello, ~a" name))

(hello "Steve")

(defun hellof (name &optional (from "The world"))
  (if from
      (format t "Hello, ~a, from ~a" name from)
      (format t "Hello, ~a" name)))

(hellof "Jim" "Alpacas")
(hellof "Jim")

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
  (format nil "Hello, ~a ~a, from ~a" honorific name from))

(generalized-greeter "Jim")
(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")

(member 'Groucho '(Harpo Groucho Zeppo))

(typecase 1
  (string :string)
  (integer :int))

(dolist (i '(1 2 3 4))
  (format t "~a" i))

(defparameter temp-two '())
(dolist (temp-one '(1 2 3 4) temp-two) (push temp-one temp-two))

;;; simple loop loops until return
(let ((*i* 5))
  (loop (if (zerop *i*)
            (return)
            (progn (setf *i* (1- *i*))
                   (print *i*)))))

;;; works but well..
;; (loop (terpri))

(let ((variable 10))
  (setf variable 2))

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
    :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg  :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

(describe 'human-powered-conveyance)
(describe 'bicycle)

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

(defmethod initialize-instance :after ((object canoe) &rest args)
  (declare (ignore args))
  (setf (average-efficiency object) (log (1+ (number-of-rowers object)))))

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
(circumference (make-instance 'bicycle :wheel-size 2))

;;; kan ej flytta ut kommana eftersom value måste evalueras, men måste
;;; kunna vara en lexikal/dynamiskt bunden variabel
(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
       ,(ecase unit
               (s 1)
               (m 60)
               (h 3600)
               (d 86400)
               (ms 1/1000)
               (us 1/1000000)))))

(sleep-units 2 s)
(defparameter *sleepytime* 1)
(sleep-units *sleepytime* s)

;;; visar att nästlad defun faktiskt definierar dynamiskt
(defun fundefun ()
  (defun hello ()
    (format t "hello")))
(fundefun)
(hello)

(defun add (x y)
  (+ x y))
(defun addop (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum x y))
  (the fixnum (+ x y)))


(defun collect-leaves (tree)
  (let ((leaves ()))
    (labels ((walk (tree)
               (cond
                 ((null tree))
                 ((atom tree) (push tree leaves))
                 (t (walk (car tree))
                    (walk (cdr tree))))))
      (walk tree))
    (nreverse leaves)))
