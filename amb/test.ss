(define gen (lambda () (define g (lambda (n max) (if (< n max) (amb n (g (+ n 1) max)) (amb max)))) (g 0 1000)))


;;;test不确定求值
(let ((a (gen))
      (b (gen)))
  (require (= 100 (+ (* a a) (* b b) (* 2 a b))))
  (display (list a b))
  (amb))
