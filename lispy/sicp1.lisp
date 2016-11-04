;;; Code is quickly spliced from my racket variant, expect some un-idiomatic usage

(defun square (x) (* x x))
(defun average (x y)
  (/ (+ x y) 2))

(defun sqr (x)
  (defun good-enough? (guess)
    (< (abs (- (square guess) x)) (/ x 10000)))
  (defun improve (guess)
    (average guess (/ x guess)))
  (defun sqrt-iter (guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (t (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(defun count-change (amount)
  (cc amount 5))

(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((evenp n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

(defun display-space (x)
  (format t "~a " x))
(defun display-all (&rest args)
  (map nil #'display-space args)
  (terpri))


(defun fast2-expt (b n)
  (fat b n 1))
(defun fat (base exp acc)
  (display-all 'fat base exp acc)
  (if (= exp 0) acc
    (if (evenp exp)
      (fat (* base base) (/ exp 2) acc)
      (fat base (- exp 1) (* acc base)))))


(defun mygcd (a b)
  (if (= b 0)
    a
    (mygcd b (rem a b))))

(defun divides? (a b)
  (= (rem b a) 0))
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))
(defun smallest-divisor (n) 
  (find-divisor n 2))
(defun prime? (n)
  (= n (smallest-divisor n)))


(smallest-divisor 10)                   ;2

(prime? 17)

(smallest-divisor 17)
(smallest-divisor 15)
(smallest-divisor 887)


(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp)
         (rem (square (expmod base (/ exp 2) m))
                    m))
        (t
          (rem (* base (expmod base (- exp 1) m))
                     m))))


(expmod 3 2 3)
(random 5)

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))

(fast-prime? 51231241 100)
(prime? 51231241)

(defun check-prob-fermat (i n rnds)
  (if (= i n) 
    -1 
    (if (not (equal (prime? i) (fast-prime? i rnds)))
      i
      (check-prob-fermat (+ i 1) n rnds))))

(check-prob-fermat 7000 8000 3)
