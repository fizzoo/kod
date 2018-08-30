(load "stdlib.lisp")

(define x 3)

(define out (open-output-file "ut"))

(write x out)

(define (f x) (+ x 1))
(define lst (list 1 2 3 4 5))
(write (map f lst) out)


(close-output-port out)
