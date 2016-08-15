
(load "utils.ss")

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

(define compile-multi
  (lambda (lst next)
    (let loop ([lst lst] [n next])
      (if (null? lst)
	  n
	  (loop (cdr lst) (compile (car lst) n))))))

(define compile
  (lambda (x next)
    (cond [(symbol? x) (list 'refer x next)]
	  [(pair? x)
	   (record-case
	    x
	    [quote (obj) (list 'constant obj x)]
	    [lambda (vars . body) (list 'close vars (compile-multi body '(return)) next)]
	    [if (test then else)
		(let ((after-then (compile then next))
		      (after-else (compile else next)))
		  (compile test `(test ,after-then ,after-else)))]
	    [set! (var x)
		  (compile x `(assign ,var ,next))]
	    [call/cc (x)
		     (let ((c `(conti (argument ,(compile x '(apply))))))
		       (if (tail? next)
			   c
			   `(frame ,next ,c)))]
	    [else
	     (let loop ([args (cdr x)]
			   [c (compile (car x) '(apply))])
		     (if (null? args)
			 (if (tail? next)
			     c
			     `(frame ,next ,c))
			 (loop (cdr args)
			       (compile (car args) `(argument ,c)))))])]
	  [else (list 'constant x next)])))

(define lookup
  (lambda (var e)
    (let next-rib ([e e])
      (let next-var ([vars (caar e)] [vals (cdar e)])
	(cond [(null? vars) (next-rib (cdr e))]
	      [(eq? var (car vars)) vals]
	      [else (next-var (cdr vars) (cdr vals))])))))

(define closure
  (lambda (vars body e)
    (list vars body e)))

(define continuation
  (lambda (s)
    (closure '(v) `(nuate ,s v) '())))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define extend
  (lambda (e vars vals)
    (cons (cons vars vals) e)))

(define VM
  (lambda (a x e r s)
    (debug-line x)
    (record-case
     x
     [halt () a]
     [refer (var x) (VM (car (lookup var e)) x e r s)]
     [constant (var x) (VM var x e r s)]
     [close (vars body x) (VM (closure vars body e) x e r s)]
     [test (then else) (if a (VM a then e r s) (VM a else e r s))]
     [assign (var x)
	     (set-car! (lookup var e) a)
	     (VM a x e r s)]
     [conti (x) (VM (continuation s) x e r s)]
     [nuate (s var) (VM (car (lookup var e)) '(return) e r s)]
     [frame (ret x) (VM a x e '() (call-frame ret e r s))]
     [argument (x) (VM a x e (cons a r) s)]
     [apply ()
	    (VM a (cadr a) (extend e (car a) r) '() s)]
     [return ()
	     (VM a (car s) (cadr s) (caddr s) (caddr (cdr s)))]
     [else (display "syntax error\n")])))

(define evaluate
  (lambda (x)
    (VM '() (compile x '(halt)) '() '() '())))

;;(display (compile '((lambda (x y) x) 3 4) '(halt)))

(display (evaluate '((lambda (x y) (set! x 56) x) 3 4)))

