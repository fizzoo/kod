(defun replace-funcall (isfirst x flist)
  (if (atom x)
      x
      (append (cond ((not (atom (car x))) `(,(replace-funcall t (car x) flist)))
                    ((and isfirst (member (car x) flist)) `(funcall ,(car x)))
                    (t `(,(car x))))
              (replace-funcall nil (cdr x) flist))))


;;; Scheme define. Note that a variable in car-position on a list is
;;; assumed to be a function, which does not 100% correspond with
;;; usage of cond and such macros.
(defmacro define (namelist &body body)
  `(defun ,(car namelist) ,(cdr namelist) ,@(mapcar
                                             (lambda (x)
                                               (replace-funcall t x (cdr
                                                                     namelist)))
                                             body)))


;; (defun cont-frac-up (n d i k)
;;   (if (< i k)
;;       (/ (funcall n i)
;;          (+ (funcall d i)
;;             (cont-frac-up n d (1+ i) k)))
;;       0))

;; (defun cont-frac (n d k)
;;   (cont-frac-up n d 1 k))

(defun cont-frac-bottoms (n d k div)
  (if (= k 0)
      div
      (cont-frac-bottoms n d (1- k) (/ (funcall n k)
                                       (+ (funcall d k)
                                          div)))))

(defun cont-frac (n d k)
  (cont-frac-bottoms n d (1- k) (/ (funcall n k)
                               (funcall d k))))

(defun cont-frac-neg-bottoms (n d k div)
  (if (= k 0)
      div
      (cont-frac-neg-bottoms n d (1- k) (/ (funcall n k)
                                           (- (funcall d k)
                                              div)))))

(defun cont-frac-neg (n d k)
  (cont-frac-neg-bottoms n d (1- k) (/ (funcall n k)
                                       (funcall d k))))

(/ 1  (cont-frac (lambda (i)
                   (declare (ignore i)) 1.0)
                 (lambda (i)
                   (declare (ignore i)) 1.0)
                 80))

(defun eulerthingy (k)
  (if (= 0 (mod (- k 2) 3))
      (* 2 (/ (1+ k) 3))
      1))

(defun generate-until (x y)
  (if (>= x y)
      nil
      (cons x (generate-until (1+ x) y))))

(defun range (x &optional y)
  (if y
      (generate-until x y)
      (generate-until 0 x)))

(range 10)
(range 2 10)

(mapcar #'eulerthingy (range 1 20))

(cont-frac (lambda (x)
             (declare (ignore x))
             1)
           #'eulerthingy
           30)


(defun tan-cf (x k)
  (cont-frac-neg (lambda (i) (if (= i 1) x (* x x)))
                 (lambda (i) (1- (* 2 i)))
                 k))

(tan-cf 0.3 400)
(tan 0.3)

(defun average (&rest all)
  (/ (apply #'+ all)
     (length all)))

(average 1)
(average 1 2)
(average 1 2 3)

(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))

(defun square (x)
  (* x x))
(average-damp #'square)

(funcall (average-damp #'square) 10)

(defconstant +dx+ 0.0001)
(define (deriva g)
  (lambda (x)
    (/ (- (g (+ x +dx+)) (g x))
       +dx+)))

(funcall (deriva #'square) 1)

(defun cube (x) (* x x x))

(funcall (deriva #'cube) 5)

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

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) (funcall (deriva g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(defun sqrtf (x)
  (newtons-method (lambda (y) (- (square y) x))
                  1))

(sqrtf 16)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(numer (make-rat 10 12))

(define (print-rat x)
  (format t "~a / ~a~%" (numer x) (denom x)))

(let ((one-half (make-rat 1 2)))
  (print-rat one-half))

(let ((one-third (make-rat 1 3)))
      (print-rat (add-rat one-third one-third)))

(defstruct ratt (numer 1) denom)

(defparameter r (make-ratt :numer 1 :denom 2))
(defparameter r (make-ratt))
(setf (ratt-numer r) 3)
(print r)


(defstruct (point
             (:constructor make-point (x y)))
  x y)

(defun x-point (p)
  (point-x p))
(defun y-point (p)
  (point-y p))

(make-point 1.0 2.0)

(defstruct segment start end)

(defun midpoint-segment (s)
  (let ((sta (segment-start s))
        (nd  (segment-end s)))
    (make-segment :start (/ (+ (x-point sta)
                               (x-point nd))
                            2)
                  :end (/ (+ (y-point sta)
                             (y-point nd))
                          2))))


(midpoint-segment (make-segment :start (make-point 0 0) :end (make-point 1 2)))

(define (my-cons x y)
  (lambda (m)
          (cond ((= m 0) x)
                ((= m 1) y)
                (t (error "Argument not 0 or 1 -- CONS")))))

(my-cons 1 2)

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

(my-car (my-cons 1 2))

(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref '(1 2 3) 1)                   ;no throw oob
(elt '(1 2 3) 1)                        ;throw oob
(length nil)

(define (mylength items)
  (define (length-iter a count)
    (if (null a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(mylength '(1 2 3))
(length (cons (cons 1 2)
              (cons 3 nil)))

(defun myreverse (x)
  (labels ((myreverse-iter (acc x)
           (if (null x)
               acc
               (myreverse-iter (cons (car x) acc) (cdr x)))))
    (myreverse-iter nil x)))

;;; sloow :'(
;; (defun myreverse (x)
;;   (if (null x)
;;       x
;;       (append (myreverse (cdr x)) (list (car x)))))

;; (myreverse '(1 2 3))
;; (myreverse nil)
;; (reverse (list 1 2 3))

(defun myrevers (x)
  (reduce #'cons x :from-end t :initial-value nil))

(myrevers '(1 2 3))
(myrevers nil)

;;; hitta foldl
(reduce (lambda (x y) (cons y x)) '(1 2 3) :initial-value nil)


(mapcar #'abs (list -10 2.5 -11.6 17))

(defun count-leaves (x)
  (if (atom x)
      (if (null x)
          0
          1)
      (+ (count-leaves (car x))
         (count-leaves (cdr x)))))

(count-leaves '(1 2 3))
(count-leaves (list (list 1 2 3)
                    (list 4 5 6)))

;;; manuell
;; (defun square-tree (x)
;;   (cond ((null x) nil)
;;         ((atom x) (* x x))
;;         (t (cons (square-tree (car x))
;;            (square-tree (cdr x))))))

;;; me map
(defun square-tree (x)
  (cond ((null x) nil)
        ((atom x) (* x x))
        (t (mapcar #'square-tree x))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(defun tree-map (fun x)
  (cond ((null x) nil)
        ((atom x) (funcall fun x))
        (t (cons (tree-map fun (car x))
           (tree-map fun (cdr x))))))

(tree-map #'square
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(defun filter (pred x)
  (cond ((null x) nil)
        ((funcall pred (car x))
         (cons (car x)
               (filter pred (cdr x))))
        (t (filter pred (cdr x)))))

(filter (lambda (x) (= 0 (mod x 2))) (range 0 10))

;;; rätt foldr/foldl enl. haskell
(defun foldr (op end sequence)
  (if (null sequence)
      end
      (funcall op (car sequence)
               (foldr op end (cdr sequence)))))

(foldr #'cons nil '(1 2 3))
(foldr #'+ 0 '(1 2 3))

;;; initial = acc, seq = rest
(defun foldl (op acc sequence)
  (if (null sequence)
      acc
      (foldl op (funcall op acc (car sequence)) (cdr sequence))))

(foldl #'cons nil '(1 2 3))
(foldl (lambda (x y)
         (append x (list y)))
       nil
       '(1 2 3))
(foldl #'+ 0 '(1 2 3))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (enumerate-tree (car tree))
                   (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(defun sum (x)
  (foldl #'+ 0 x))

(defun sum-odd-squares (tree)
  (sum (mapcar (lambda (x) (* x x))
               (filter (lambda (x) (= 0 (mod x 2)))
                       (enumerate-tree tree)))))

(defun mymap (p sequence)
  (foldr (lambda (x y) (cons (funcall p x) y)) nil sequence))

(mymap (lambda (x) (* 2 x))
       '(1 2 3))

(defun myappend (seq1 seq2)
  (foldr #'cons seq2 seq1))

(myappend '(1 2) '(3 4))

(defun mylengthb (seq)
  (foldr (lambda (x y) (declare (ignore x)) (+ 1 y)) 0 seq))

(mylengthb '(1 2 3 4))
(mylengthb '(1))
(mylengthb '())

(defun horner-eval (x coefficient-sequence)
  (foldr (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(defun foldr-n (op init seqs)
    (if (null (car seqs))
        nil
        (cons (foldr op init (mapcar #'car seqs))
              (foldr-n op init (mapcar #'cdr seqs)))))

(foldr-n #'+ 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(foldr #'/ 1 (list 1 2 3))
(foldl #'/ 1 (list 1 2 3))
(foldr #'list nil (list 1 2 3))
(foldl #'list nil (list 1 2 3))


(defun flatmap (proc seq)
  (foldr #'append nil (mapcar proc seq)))

(mapcar (function list) (list 1 2 3))

(flatmap (lambda (x) (list (1+ x))) (list 1 2 3))

(flatmap (lambda (i)
           (mapcar (lambda (j) (list i j))
                (range 1 i)))
         (range 6))

(defun prime (x)
  (some (lambda (y) (= x y))
        (list 1 2 3 5 7 11 13 17 19 23 29)))
(define (prime-sum? pair)
  (prime (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
       (filter #'prime-sum?
               (flatmap
                (lambda (i)
                  (mapcar (lambda (j) (list i j))
                       (range 1 i)))
                (range 1 n)))))

(prime-sum-pairs 6)

(define (permutations s)
  (if (null s)
      (list nil)
      (flatmap (lambda (x)
                 (mapcar (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))
(permutations nil)

(mapcar #'+ (list 1 2 3) (list 40 50 60) (list 700 800 900))

(eq 'a 'a)
(eql 'a 'a)
(equal 'a 'a)

(defun memq (item x)
  (cond ((null x) nil)
        ((eq item (car x)) x)
        (t (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(pear apple banan))

(eq '(ha) '(ha))
(equal '(ha) '(ha))

(car ''abraca)

(defun variable? (x) (symbolp x))
(defun number? (x) (numberp x))
(defvar else t)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (equal v1 v2)))


(define (=number? exp num)
  (and (numberp exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (listp x) (eq (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (listp x) (eq (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(defun deriv (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (t
         (error "unknown expression type -- DERIV"))))

(numberp '(+ x 3))
(variable? '(+ x 3))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
