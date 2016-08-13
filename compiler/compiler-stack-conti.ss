;;instructions :
;;(halt)
;;(refer n m x)
;;(constant obj x)
;;(close body x)
;;(test then else)
;;(assign var x)
;;(conti x)
;;(nuate stack x)
;;(frame x ret)
;;(argument x) => push the argument in acc on stack
;;(apply)
;;(return)

;; the stack' structures
;; | the next expression (top)  -> x
;; | the current environment    -> e
;; | the current rib (bottom)   -> r

(load "utils.ss")
(load "stack.ss")

(define closure
  (lambda (body e)
    (list body e)))

(define continuation
  (lambda (s)
    (closure `(refer ,0 ,0 (nuate ,(save-stack s) (return))) '())))

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

(define compile-lookup
  (lambda (var e return)
    (let next-rib ([e e] [rib 0])
      (let next-elt ([vars (car e)] [elt 0])
	(cond
	 [(null? vars) (next-rib (cdr e) (+ rib 1))]
	 [(eq? (car vars) var) (return rib elt)]
	 [else (next-elt (cdr vars) (+ elt 1))])))))

(define (extend e r)
  (cons r e))

(define compile
  (lambda (x e next)
    ;;(debug-line x)
    (cond
     [(symbol? x) (compile-lookup x e (lambda (n m)
					`(refer ,n ,m ,next)))]
     [(pair? x)
      (record-case
       x
       [quote (obj) `(constant ,obj ,next)]
       [lambda (vars body)
	 `(close ,(compile body (extend e vars) `(return)) ,next)]
       [if (test then else)
	   (let ([after-then (compile then e next)]
		 [after-else (compile else e next)])
	     (compile test e `(test ,after-then, after-else)))]
       [set! (var x)
	     (compile-lookup var e (lambda (n m)
				     (compile x e `(assign ,n ,m ,next))))]
       [call/cc (x)
		(let ((c `(conti (argument ,(compile x e '(apply))))))
		  (if (tail? next)
		      c
		      `(frame ,next ,c)))]
       [else
	(let loop ([args (cdr x)] [c (compile (car x) e '(apply))])
	  (if (null? args)
	      (if (tail? next)
		  c
		  `(frame ,next ,c))
	      (loop (cdr args)
		    (compile (car args)
			     e
			     `(argument ,c)))))])]
     [else `(constant ,x ,next)])))

(define lookup
  (lambda (n m e)
    (let next-rib ([e e] [rib n])
      (if (= rib 0)
	  (let next-elt ([r (car e)] [elt m])
	    (if (= elt 0)
		r
		(next-elt (cdr r) (- elt 1))))
	  (next-rib (cdr e) (- rib 1))))))

(define VM
  (lambda (a x e r s)
    (record-case
     x
     [halt () a]
     [refer (n m x) (VM (car (lookup n m e)) x e r s)]
     [constant (obj x) (VM obj x e r s)]
     [close (body x) (VM (closure body e) x e r s)]
     [test (then else) (VM a (if a then else) e r s)]
     [assign (n m x) (set-car! (lookup n m e) a) (VM a x e r s)]
     [conti (x) (VM (continuation s) x e r s)]
     [nuate (stack x) (VM a x e r (restore-stack stack))]
     [frame (ret x) (VM a x e '() (push ret (push e (push r s))))]
     [argument (x) (VM a x e (cons a r) s)]
     [apply () (VM a (car a) (extend (cadr a) r) '() s)]
     [return () (VM a (index s 0) (index s 1) (index s 2) (- s 3))]
     [else (display "syntax error\n")])))


(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) '() '() 0)))

;;(display (compile '((lambda (x) (call/cc (lambda (k) (k 2)))) 1) '() '(halt)))

(display (evaluate '((lambda (x) (call/cc (lambda (k) (k 100)))) 1)))

