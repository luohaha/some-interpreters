;;instructions :
;;(halt)
;;(refer var x)
;;(constant obj x)
;;(close body x)
;;(test then else)
;;(assign var x)
;;(frame x ret)
;;(argument x) => push the argument in acc on stack
;;(apply)
;;(return n)

;; the stack' structures
;; | static link (top)
;; | first arg
;; | second arg
;; | ...
;; | ...
;; | dynamic link
;; | last call frame's static link (bottom)

(load "utils.ss")
(load "stack.ss")

(define functional
  (lambda (body e)
    (list body e)))

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
	 `(close ,(compile body (extend e vars) `(return ,(+ (length vars) 1))) ,next)]
       [if (test then else)
	   (let ([after-then (compile then e next)]
		 [after-else (compile else e next)])
	     (compile test e `(test ,after-then, after-else)))]
       [set! (var x)
	     (compile-lookup var e (lambda (n m)
				     (compile x e `(assign ,n ,m ,next))))]
       [else
	(let loop ([args (cdr x)] [c (compile (car x) e '(apply))])
	  (if (null? args)
	      `(frame ,c ,next)
	      (loop (cdr args)
		    (compile (car args)
			     e
			     `(argument ,c)))))])]
     [else `(constant ,x ,next)])))

;;find the nth static frame starting with e
(define find-link
  (lambda (n e)
    (if (= n 0)
	e
	(find-link (- n 1) (index e -1)))))

(define VM
  (lambda (a x e s)
    ;;(debug-line a)
    ;;(debug-line x)
    ;;(display "===\n")
    (record-case
     x
     [halt () a]
     [refer (n m x) (VM (index (find-link n e) m) x e s)]
     [constant (obj x) (VM obj x e s)]
     [close (body x) (VM (functional body e) x e s)]
     [test (then else) (VM a (if a then else) e s)]
     [assign (n m x)
	     (index-set! (find-link n e) m a)
	     (VM a x e s)]
     [frame (x ret) (VM a x e (push ret (push e s)))]
     [argument (x) (VM a x e (push a s))]
     [apply () (VM a (car a) s (push (cadr a) s))]
     [return (n)
	     (let ((s (- s n)))
	       (VM a (index s 0) (index s 1) (- s 2)))]
     [else (display "syntax error!\n")])))

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) 0 0)))

;;(display (compile '((lambda (x) x) 1) '() '(halt)))

(display (evaluate '((lambda (x) ((lambda (y) y) x)) 1)))

