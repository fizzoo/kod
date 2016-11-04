(defun fizzbuzz (i)
  (cond
    ((zerop (mod i 15))
     "fizzbuzz")
    ((zerop (mod i 5))
     "buzz")
    ((zerop (mod i 3))
     "fizz")
    (t i)))

(disassemble 'fizzbuzz)

(declaim (inline declared-fizzbuzz))
(defun declared-fizzbuzz (i)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           ((unsigned-byte 62) i))
  (cond
    ((zerop (mod i 15))
     "fizzbuzz")
    ((zerop (mod i 5))
     "buzz")
    ((zerop (mod i 3))
     "fizz")
    (t i)))

(disassemble 'declared-fizzbuzz)


(time (loop for i from 1 to 1000000
         do (fizzbuzz i)))

(time (loop for i from 1 to 1000000
         do (declared-fizzbuzz i)))
