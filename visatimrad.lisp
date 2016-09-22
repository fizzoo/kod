
(defun sum (x)
  (apply #'+ x))

(+ 1 2 3 4)
(+ '(1 2 3 4))
(apply #'/ '(1 2 3 4))

(/ 1 2 3 4)
(- 1 (+ 2 3 4))
(/ 1 (* 2 3 4))

(if "hej" t nil)

(defun vowel (v)
  (find v '("a" "o" "u" "e" "i" "y")))

(defun strtolist (str)
  (map 'list (lambda (x) (string x)) str))

(defun rovarsprak (str)
  "translate normal text to rovarsprak"
  (if str
      (let ((s (strtolist str)))
        (if (vowel (first s))
            (cons (first s)
                  (rovarsprak (rest s)))
            (append (list (first s)
                          "o"
                          (first s))
                    (rovarsprak (rest s)))))
      nil))

(rovarsprak "hej")

(defun vowel (x)
  (member-if (lambda (l) (string-equal x l)) '("a" "o" "u" "e" "i" "y")))

(defun list-of-chars-to-string (x)
  (format nil "~{~a~}"
          (map 'list
               (lambda (l) (string l))
               x)))

(list-of-chars-to-string '(#\o #\i))

(apply #'append '((#\x) (#\o #\j)))

(defun rovare (str)
  (list-of-chars-to-string
   (apply
    #'append
    (map 'list
         (lambda (x) (if (vowel x)
                         (list x)
                         (list x #\o x)))
         str))))

(rovare "hej")

(map 'string (lambda (x) (string x)) '(#\h #\o))
