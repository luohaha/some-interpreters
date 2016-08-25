(load "boolean.ss")
(load "number.ss")

(define _cons
  (lambda (x)
    (lambda (y)
      (lambda (f)
	((f x) y)))))

(define _car
  (lambda (p)
    (p (lambda (x)
	 (lambda (y)
	   x)))))

(define _cdr
  (lambda (p)
    (p (lambda (x)
	 (lambda (y)
	   y)))))

(to-integer (_cdr ((_cons one) two)))

