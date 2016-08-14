;;instructions :
;;(halt)
;;(refer-local n x)  =>  load the nth argument in current call frame
;;(refer-free n x)  =>  load the nth free argument in closure
;;(constant obj x)
;;(close n body x) => 
;;(test then else)
;;(assign var x)
;;(conti x)
;;(nuate stack x)
;;(frame x ret)
;;(argument x) => push the argument in acc on stack
;;(apply)
;;(return n)

;; the stack' structures
;; | ...
;; | argn
;; | the next expression (top)  -> x
;; | the current frame    -> f
;; | the current closure (bottom)   -> c

;;register =>
;; a => accumulator
;; x => next expression
;; f => current call frame
;; c => current closure
;; s => current stack

(load "utils.ss")
(load "stack.ss")
(load "set.ss")

(define continuation
  (lambda (s)
    (closure `(refer-local ,0 (nuate ,(save-stack s) (return ,0))) 0 '())))

(define find-free
  (lambda (x b)
    (cond [(symbol? x) (if (set-member? x b) '() (list x))]
	  [(pair? x)
	   (record-case
	    x
	    [quote (obj) '()]
	    [lambda (vars body) (find-free body (set-union vars b))]
	    [if (test then else)
		(set-union (find-free test b)
			   (set-union (find-free then b)
				      (find-free else b)))]
	    [call/cc (clo) (find-free clo b)]
	    [else (let loop ([x x])
		    (if (null? x)
			'()
			(set-union (find-free (car x) b)
				   (loop (cdr x)))))])]
	  [else '()])))

(define compile-lookup
  (lambda (x e return-local return-free)
    (let loop-local ([locals (car e)] [n 0])
      (if (null? locals)
	  (let loop-free ([frees (cdr e)] [m 0])
	    (if (eq? (car frees) x)
		(return-free m)
		(loop-free (cdr frees) (+ m 1))))
	  (if (eq? (car locals) x)
	      (return-local n)
	      (loop-local (cdr locals) (+ n 1)))))))

(define compile-refer
  (lambda (x e next)
    (compile-lookup x e (lambda (n) `(refer-local ,n ,next))
		    (lambda (n) `(refer-free ,n ,next)))))

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
	next
	(collect-free (cdr vars) e (compile-refer (car vars)
						  e
						  `(argument ,next))))))

(define compile
  (lambda (x e next)
    ;;(debug-line x)
    (cond
     [(symbol? x) (compile-refer x e next)]
     [(pair? x)
      (record-case
       x
       [quote (obj) `(constant ,obj ,next)]
       [lambda (vars body)
	 (let ([free (find-free body vars)])
	   (collect-free free e
			 `(close ,(length free)
				 ,(compile body
					  (cons vars free)
					  `(return ,(length vars)))
				 ,next)))]
       [if (test then else)
	   (let ([after-then (compile then e next)]
		 [after-else (compile else e next)])
	     (compile test e `(test ,after-then, after-else)))]
       [call/cc (x)
		(let ((c `(conti (argument ,(compile x e '(apply))))))
		  `(frame ,next ,c))]
       [else
	(let loop ([args (cdr x)] [c (compile (car x) e '(apply))])
	  (if (null? args)
	      `(frame ,next ,c)
	      (loop (cdr args)
		    (compile (car args)
			     e
			     `(argument ,c)))))])]
     [else `(constant ,x ,next)])))

(define closure
  (lambda (body n s)
    (let ([v (make-vector (+ n 1))])
      (vector-set! v 0 body)
      (let loop ([i 0])
	(if (= i n)
	    v
	    (begin (vector-set! v (+ i 1) (index s i))
		   (loop (+ i 1))))))))

(define closure-index
  (lambda (clo n)
    (vector-ref clo n)))

(define VM
  (lambda (a x f c s)
    ;;(debug-line x)
    (record-case
     x
     [halt () a]
     [refer-local (n x) (VM (index f n) x f c s)]
     [refer-free (n x) (VM (closure-index c (+ n 1)) x f c s)]
     [constant (obj x) (VM obj x f c s)]
     [close (n body x) (VM (closure body n s) x f c (- s n))]
     [test (then else) (VM a (if a then else) f c s)]
     [conti (x) (VM (continuation s) x f c s)]
     [nuate (stack x) (VM a x f c (restore-stack stack))]
     [frame (ret x) (VM a x f c (push ret (push f (push c s))))]
     [argument (x) (VM a x f c (push a s))]
     [apply () (VM a (closure-index a 0) s a s)]
     [return (n)
	     (let ([s (- s n)])
	       (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]
     [else (display "syntax error\n")])))


(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) 0 '() 0)))

;;(display (compile '((lambda (x) (call/cc (lambda (k) (k 2)))) 1) '() '(halt)))

(display (evaluate '((lambda (x) (call/cc (lambda (k) (k x)))) 1)))


