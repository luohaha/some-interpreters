(define true
  (lambda (x)
    (lambda (y)
      x)))

(define false
  (lambda (x)
    (lambda (y)
      y)))

(define _if
  (lambda (c)
    c))

(define to-boolean
  (lambda (p)
    (((_if p) #t) #f)))


(to-boolean false)
