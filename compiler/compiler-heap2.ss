
(load "utils.ss")

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

;;find out the var's position while compiling
(define compile-lookup
  (lambda (var e)
    (let next-rib ([e e] [rib 0])
      (let next-var ([vars (car e)] [elt 0])
	(cond [(null? vars) (next-rib (cdr e) (+ 1 rib))]
	      [(eq? var (car vars)) (cons rib elt)]
	      [else (next-var (cdr vars) (+ elt 1))])))))

(define extend
  (lambda (e rib)
    (cons rib e)))

(define compile-multi
  (lambda (lst e next)
    (let loop ([lst lst] [n next])
      (if (null? lst)
	  n
	  (loop (cdr lst) (compile (car lst) e n))))))

(define compile
  (lambda (x e next)
    (cond [(symbol? x) (list 'refer (compile-lookup x e) next)]
	  [(pair? x)
	   (record-case
	    x
	    [quote (obj) (list 'constant obj x)]
	    [lambda (vars . body) (list 'close (compile-multi body (extend e vars) '(return)) next)]
	    [if (test then else)
		(let ((after-then (compile then e next))
		      (after-else (compile else e next)))
		  (compile test e `(test ,after-then ,after-else)))]
	    [set! (var x)
		  (let ((pos (compile-lookup var e)))
		    (compile x e `(assign ,pos ,next)))]
	    [call/cc (x)
		     (let ((c `(conti (argument ,(compile x e '(apply))))))
		       (if (tail? next)
			   c
			   `(frame ,next ,c)))]
	    [else
	     (let loop ([args (cdr x)]
			   [c (compile (car x) e '(apply))])
		     (if (null? args)
			 (if (tail? next)
			     c
			     `(frame ,next ,c))
			 (loop (cdr args)
			       (compile (car args) e `(argument ,c)))))])]
	  [else (list 'constant x next)])))

(define closure
  (lambda (body e)
    (list body e)))

(define continuation
  (lambda (s)
    (closure `(nuate ,s (0 . 0)) '())))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define lookup
  (lambda (access e)
    (let next-rib ([e e] [rib (car access)])
      (if (= rib 0)
	  (let next-elt ([elt (cdr access)] [r (car e)])
	    (if (= elt 0)
		r
		(next-elt (- elt 1) (cdr r))))
	  (next-rib (cdr e) (- rib 1))))))

(define VM
  (lambda (a x e r s)
    ;;(debug-line x)
    (record-case
     x
     [halt () a]
     [refer (var x) (VM (car (lookup var e)) x e r s)]
     [constant (var x) (VM var x e r s)]
     [close (body x) (VM (closure body e) x e r s)]
     [test (then else) (if a (VM a then e r s) (VM a else e r s))]
     [assign (var x)
	     (set-car! (lookup var e) a)
	     (VM a x e r s)]
     [conti (x) (VM (continuation s) x e r s)]
     [nuate (s var) (VM (car (lookup var e)) '(return) e r s)]
     [frame (ret x) (VM a x e '() (call-frame ret e r s))]
     [argument (x) (VM a x e (cons a r) s)]
     [apply ()
	    (VM a (car a) (extend (cadr a) r) '() s)]
     [return ()
	     (VM a (car s) (cadr s) (caddr s) (caddr (cdr s)))]
     [else (display "syntax error\n")])))

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) '() '() '())))

;;(display (compile '((lambda (x y) x) 3 4) '() '(halt)))

(display (evaluate '((lambda (x y) (set! x 45) x) 3 4)))


