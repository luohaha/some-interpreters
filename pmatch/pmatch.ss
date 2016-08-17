
(define-syntax qp
  (syntax-rules (unquote)
    [(_ v () kt kf) (if (null? v) kt kf)]
    [(_ v (unquote var) kt kf)
     (let ((var v)) kt)]
    [(_ v (x . y) kt kf)
     (if (pair? v)
	 (let ((first (car v)) (second (cdr v)))
	   (qp first x (qp second y kt kf) kf))
	 kf)]
    [(_ v literal kt kf)
     (if (equal? v 'literal) kt kf)]))
(define-syntax ppat
  (syntax-rules (quasiquote unquote quote ? pand por pnot *)
    [(_ v () kt kf) (if (null? v) kt kf)]
    [(_ v * kt kf) kt]
    [(_ v (quote lst) kt kf)
     (if (equal? v (quote lst)) kt kf)]
    [(_ v (quasiquote lst) kt kf)
     (qp v lst kt kf)]
    [(_ v (unquote lst) kt kf)
     (let ((lst v)) kt)]
    [(_ v (? expr pat) kt kf)
     (if (expr v)
	 (ppat v pat kt kf)
	 kf)]
    [(_ v (pand pat pat1 ...) kt kf)
     (if (null? (list pat1 ...))
	 (ppat v pat kt kf)
	 (ppat v pat (ppat v (pand pat1 ...) kt kf) kf))]
    [(_ v (por pat pat1 ...) kt kf)
     (if (null? (list pat1 ...))
	 (ppat v pat kt kf)
	 (ppat v pat kt (ppat v (por pat1 ...) kt kf)))]
    [(_ v (pnot pat pat1 ...) kt kf)
     (if (null? (list pat1 ...))
	 (ppat v pat kf kt)
	 (ppat v pat kf (ppat v (pnot pat1 ...) kt kf)))]
    [(_ v (x . y) kt kf)
     (if (pair? v)
	 (let ((first (car v)) (second (cdr v)))
	   (ppat first x (ppat second y kt kf) kf))
	 kf)]
    [(_ v literal kt kf)
     (if (equal? v literal) kt kf)]))

(define-syntax pmatch
  (syntax-rules (else guard)
    [(_ (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch v cs ...))]
    [(_ v)
     (begin (display "match failed: ") (display v) (newline))]
    [(_ v (else a ...))
     (let ()
       a ...)]
    [(_ v (pattern (guard g ...) a ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pattern (if (and g ...) (let () a ...) (fk)) (fk)))]
    [(_ v (pattern a ...) cs ...)
     (let ((fk (lambda () (pmatch v cs ...))))
       (ppat v pattern (let () a ...) (fk)))]))
