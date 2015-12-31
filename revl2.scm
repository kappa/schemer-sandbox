(define revl
  (lambda (l)
	(revl_aux l '())))

(define revl_aux
  (lambda (l a)
	(cond
	  ((null? (cdr l)) (cons (car l) a))
	  (else (revl_aux (cdr l) (cons (car l) a))))))

(define revl_co
  (lambda (l)
	(revl_co_aux l (lambda () '()))))

(define revl_co_aux
  (lambda (l f)
	(cond
	  ((null? l) (f))
	  (else (revl_co_aux (cdr l) (lambda ()
								   (cons (car l) (f))))))))
