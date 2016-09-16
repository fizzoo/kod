(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) (/ x 10000)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount(- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (display-all . args)
  (for-each display-space args)
  (newline))

(define (display-space x)
  (display x)
  (display " "))


(define (fat base exp acc)
  (display-all 'fat base exp acc)
  (if (= exp 0) acc
    (if (even? exp)
      (fat (* base base) (/ exp 2) acc)
      (fat base (- exp 1) (* acc base)))))
(define (fast2-expt b n)
  (fat b n 1))


(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (smallest-divisor n) 
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

; ska funka

(smallest-divisor 10)
;2

(prime? 17)

(smallest-divisor 17)
(smallest-divisor 15)
(smallest-divisor 887)


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))


(expmod 3 2 3)
(random 5)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 51231242 5)
(prime? 51231242)

(define (check-prob-fermat i n rnds)
  (if (= i n) 
    -1 
    (if (not (eq? (prime? i) (fast-prime? i rnds)))
      i
      (check-prob-fermat (+ i 1) n rnds))))

(check-prob-fermat 7000 8000 3)

(eq? #t #t)

