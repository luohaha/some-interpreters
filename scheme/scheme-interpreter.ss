(define-syntax error
  (syntax-rules ()
    ((_ x)
     (display x))
    ((_ a b ...)
     (begin (display a)
	    (error b ...)))))

(define the-empty-env '())

(define (first-frame env)
  (car env))

(define (enclosing-env env)
  (cdr env))

;;make变量框架
(define (make-frame vars vals)
  (cons vars vals))

(define (frame-vars frame)
  (car frame))

(define (frame-vals frame)
  (cdr frame))

;;添加var和val到新框架
(define (add-to-frame var val frame)
  (set-car! frame (cons var (frame-vars frame)))
  (set-cdr! frame (cons val (frame-vals frame))))

;;拓展环境
(define (extend-env vars vals env)
  (if (= (length vars)
	 (length vals))
      (cons (make-frame vars vals) env)
      (error "Exception : vars and vals don't match : " vars vals)))

;;查找变量
(define (lookup-var-value var env)
  (define (loop-env env)
    (define (scan vars vals)
      (cond ((null? vars) (loop-env (cdr env)))
	    ((eq? var (car vars)) (car vals))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-env)
	(error "Exception: variable " var " is not bound")
	(let ((frame (first-frame env)))
	  (scan (frame-vars frame)
		(frame-vals frame)))))
  (loop-env env))

;;设置变量
(define (set-variable-value! var val env)
  (define (loop-env env)
    (define (scan vars vals)
      (cond ((null? vars) (loop-env (cdr env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-env)
	(error "Exception: variable " var " is not bound")
	(let ((frame (first-frame env)))
	  (scan (frame-vars frame)
		(frame-vals frame)))))
  (loop-env env))

;;定义变量
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-to-frame var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame)
	  (frame-vals frame))))

;;定义多个变量
(define (define-variable*! vars vals env)
  (define (loop-var vars vals)
    (if (not (null? vars))
	(let ((var (car vars))
	  (val (car vals)))
	  (if (not (null? var))
	      (begin (define-variable! var val env)
		     (loop-var (cdr vars) (cdr vals)))))))
  (if (= (length vars)
	 (length vals))
      (loop-var vars vals)
      (error "vars and vals don't match: " vars " " vals)))

;;原始函数
(define primitive-procedures
  (list (list 'car car) (list 'cdr cdr) (list 'cons cons)
	(list 'null? null?) (list '= =) (list '> >)
	(list '< <) (list 'list list) (list 'not not)
	(list 'display display)
	(list 'boolean? boolean?)
	(list 'number? number?)
	(list 'string? string?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '#t #t)
	(list '#f #f)
	(list 'newline newline)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
	 (list 'primitive (cadr proc)))
       primitive-procedures))

;;设置初始化的环境
(define (setup-env)
  (let ((init-env (extend-env (primitive-procedure-names)
			      (primitive-procedure-objects)
			      the-empty-env)))
    init-env))

;;全局环境
(define the-global-env (setup-env))

;;执行args
(define (get-args args env succeed fail)
  ;;(error (car args) " " (cdr args))
    (if (null? args)
	(succeed '() fail)
	((car args) env (lambda (val fail2)
			  (get-args (cdr args) env (lambda (val2 fail3)
						     (succeed (cons val val2)
							      fail3))
				    fail2))
	 fail)))

;;执行原始函数
(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

(define (tagged-list? exp tag)
  (if (eq? (car exp) tag)
      #t
      #f))
(define (amb? exp)
  (tagged-list? exp 'amb))

(define (get-amb-choice exp)
  (cdr exp))

;;自求值
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;;求quoted
(define (analyze-quoted exp)
  (let ((qval (cdr exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;;求var
(define (analyze-var exp)
  (lambda (env succeed fail)
    (succeed (lookup-var-value exp env) fail)))

;;求值序列
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env (lambda (val fail2)
	       (b env succeed fail2))
	 fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "empty sequence!!!")
	(loop (car procs) (cdr procs)))))

;;构造procedure
(define (make-procedure params body env)
  (list 'procedure params body env))

;;求lambda
(define (analyze-lambda exp)
  (let ((vars (cadr exp))
	(bproc (analyze-sequence (cdr (cdr exp)))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
	       fail))))

;;求if
(define (analyze-if exp)
  (let ((pproc (analyze (cadr exp)))
	(cproc (analyze (caddr exp))))
    (if (null? (cdr (cdr (cdr exp))))
	(lambda (env succeed fail)
	  (pproc env
		 (lambda (p-val fail2)
		   (if p-val
		       (cproc env succeed fail2)))
		 fail))
	(let ((aproc (analyze (car (cdr (cdr (cdr exp)))))))
	  (lambda (env succeed fail)
	    (pproc env
		   (lambda (p-val fail2)
		     (if p-val
			 (cproc env succeed fail2)
			 (aproc env succeed fail2)))
		   fail))))))

;;求定义
(define (analyze-definition exp)
  (let ((var (cadr exp))
	(vproc (analyze (caddr exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (define-variable! var val env)
	       (succeed 'ok fail2))
	     fail))))

;;defun 转到 define
(define (defun->define exp)
  `(define ,(car (cadr exp))
     (lambda ,(cdr (cadr exp))
       ,@(cddr exp))))

;;命名let转为普通let
(define (name-let->let exp)
  `(let ,(caddr exp)
     (define ,(cadr exp)
       (lambda ,(map car (caddr exp))
	 ,@(cdr (cdr (cdr exp)))))
     (,(cadr exp) ,@(map cadr (caddr exp)))))

;;求函数定义
(define (analyze-defun exp)
  (analyze (defun->define exp)))

(define (analyze-let exp)
  (let ((body (analyze-sequence (cddr exp)))
	(params (map car (cadr exp)))
	(args (map analyze (map cadr (cadr exp)))))
    (lambda (env succeed fail)
      (get-args args env
		(lambda (val fail2)
		  (body (extend-env params val env)
			(lambda (val2 fail3)
			  (succeed val2 fail3))
			fail2))
		fail))))

(define (analyze-name-let exp)
  (analyze (name-let->let exp)))

(define (analyze-let* exp)
  (let ((body (analyze-sequence (cddr exp)))
	(params (map car (cadr exp)))
	(args (map analyze (map cadr (cadr exp)))))
    (define (loop-arg params args now-env succeed fail)
      (if (null? params)
	  (body now-env (lambda (val fail2)
			  (succeed val fail2))
		fail)
	  ((car args) now-env (lambda (val fail2)
				(loop-arg (cdr params)
					  (cdr args)
					  (extend-env (list (car params))
						      (list val)
						      now-env)
					  succeed
					  fail))
	   fail)))
    (lambda (env succeed fail)
      (loop-arg params args env succeed fail))))

(define (analyze-letrec exp)
  (let ((body (analyze-sequence (cddr exp)))
	(params (map car (cadr exp)))
	(args (map analyze (map cadr (cadr exp)))))
    (lambda (env succeed fail)
      (let ((after-env (extend-env (list '())
				   (list '())
				   env)))
	(get-args args after-env
		  (lambda (val fail2)
		    (define-variable*! params val after-env)
		    (body after-env
			  (lambda (val2 fail3)
			    (succeed val2 fail3))
			  fail2))
		  fail)))))

;;求赋值
(define (analyze-assignment exp)
  (let ((var (cadr exp))
	(vproc (analyze (caddr exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (val fail2)
	       (set-variable-value! var val env)
	       (succeed 'ok fail2))
	     fail))))

;;是否是原始函数
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

;;是否是自定义函数
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

;;apply
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
	 (succeed (apply-primitive-procedure proc args) fail))
	((compound-procedure? proc)
	 ((caddr proc) (extend-env (cadr proc) args (caddr (cdr proc))) succeed fail))
	(else
	 (error "unknow procedure"))))

;;求值过程
(define (analyze-application exp)
  (let ((fproc (analyze (car exp)))
	(aprocs (map analyze (cdr exp))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (val fail2)
	       (get-args aprocs
			 env
			 (lambda (args fail3)
			   (execute-application val args succeed fail3))
			 fail2))
	     fail))))
;;用于amb回朔
(define the-fail (lambda () 'fail))

;;分析amb
(define (analyze-amb exp)
  (let ((chois (map analyze (get-amb-choice exp))))
    (lambda (env succeed fail)
      (define (try-next chois)
	(if (null? chois)
	    (fail)
	    (begin (set! the-fail (lambda () (try-next (cdr chois))))
		   ((car chois) env succeed (lambda ()
				       (try-next (cdr chois)))))))
      (if (null? (get-amb-choice exp))
	  (the-fail)
	  (try-next chois)))))

;;分析require
(define (analyze-require exp)
  (let ((pproc (analyze (cadr exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value fail2)
	       (if pred-value
		   (succeed 'ok fail2)
		   (the-fail)))
	     fail))))
;;call/cc
(define (analyze-call/cc exp)
  (let ((call (analyze (cadr exp))))
    (lambda (env succeed fail)
      (call env
	    (lambda (val fail2)
	      (get-args (list (lambda (e s f)
			  (s (list 'primitive succeed) f)))
			env
			(lambda (args fail3)
			  (execute-application val args succeed fail3))
			fail2))
	    fail))))

;;把cond转化为if
(define (cond->if exp)
  (define (make-if first rest)
    (if (not (null? rest))
	`(if ,(car first)
	 (begin ,@(cdr first))
	 ,(make-if (car rest) (cdr rest)))
	(if (eq? (car first) 'else)
	    `(begin ,@(cdr first))
	    `(if ,(car first)
	     (begin ,@(cdr first))))))
  (if (not (null? (cdr exp)))
      (let ((first (car (cadr exp)))
	    (rest (cdr (cadr exp))))
	(make-if first rest))))

(define (analyze-cond exp)
  (analyze (cond->if exp)))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	((boolean? exp) #t)
	(else #f)))

(define (var? exp)
  (symbol? exp))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (if? exp)
  (tagged-list? exp 'if))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (define? exp)
  (tagged-list? exp 'define))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (application? exp)
  (pair? exp))

(define (require? exp)
  (tagged-list? exp 'require))

(define (let? exp)
  (tagged-list? exp 'let))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (call/cc? exp)
  (tagged-list? exp 'call/cc))

(define (analyze exp)
  (cond ((self-evaluating? exp)
	 (analyze-self-evaluating exp))
	((var? exp)
	 (analyze-var exp))
	((lambda? exp)
	 (analyze-lambda exp))
	((if? exp)
	 (analyze-if exp))
	((assignment? exp)
	 (analyze-assignment exp))
	((define? exp)
	 (if (pair? (cadr exp))
	     (analyze-defun exp)
	     (analyze-definition exp)))
	((amb? exp)
	 (analyze-amb exp))
	((require? exp)
	 (analyze-require exp))
	((let? exp)
	 (if (pair? (cadr exp))
	     (analyze-let exp)
	     (analyze-name-let exp)))
	((cond? exp)
	 (analyze-cond exp))
	((call/cc? exp)
	 (analyze-call/cc exp))
	((let*? exp)
	 (analyze-let* exp))
	((letrec? exp)
	 (analyze-letrec exp))
	((quoted? exp)
	 (analyze-quoted exp))
	((begin? exp)
	 (analyze-sequence (cdr exp)))
	((application? exp)
	 (analyze-application exp))
	(else
	 (error "error type!!!" exp))))

;;eval
(define (amb-eval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;读取文件
(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((ls '())
		 (s (read)))
	(if (eof-object? s)
	    (reverse ls)
	    (loop (cons s ls) (read)))))))

(define call/cc call-with-current-continuation)

(define (my-eval exp)
  (call/cc
   (lambda (k)
     (amb-eval exp the-global-env
	    (lambda (val fail)
	      (k val))
	    (lambda ()
	      'fail)))))
;;解释文件中的内容
(define (eval-file filename)
  (define (eval-loop lst ans)
    (if (not (null? lst))
	(eval-loop (cdr lst)
		   (my-eval (car lst)))
	ans))
  (eval-loop (read-file filename) '()))
