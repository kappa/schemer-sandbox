(define multirember_co
  (lambda (lat a f)
	(cond
	  ((null? lat) (f '() '()))
	  ((eq? (car lat) a) 
	    (multirember_co (cdr lat) a (lambda (l1 l2)
									 (f (cons (car lat) l1) l2))))
	  (else
		(multirember_co (cdr lat) a (lambda (l1 l2)
									  (f l1 (cons (car lat) l2))))))))
