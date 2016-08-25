
(define zero
  (lambda (p)
    (lambda (x)
      x)))

(define one
  (lambda (p)
    (lambda (x)
      (p x))))

(define two
  (lambda (p)
    (lambda (x)
      (p (p x)))))

(define to-integer
  (lambda (p)
    ((p (lambda (x) (+ x 1))) 0)))

(to-integer two)
