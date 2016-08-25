(load "number.ss")
(load "boolean.ss")
(load "pair.ss")

(define increment
  (lambda (n)
    (lambda (p)
      (lambda (x)
	(p ((n p) x))))))

(define slide
  (lambda (p)
    ((_cons (_cdr p)) (increment (_cdr p)))))

(define decrement
  (lambda (n)
    (_car ((n slide) ((_cons zero) zero)))))

(define add
  (lambda (m)
    (lambda (n)
      ((n increment) m))))

(define sub
  (lambda (m)
    (lambda (n)
      ((n decrement) m))))

(define mul
  (lambda (m)
    (lambda (n)
      ((n (add m)) zero))))

(define pow
  (lambda (m)
    (lambda (n)
      ((n (mul m)) one))))

(to-integer ((pow two) (increment two)))
