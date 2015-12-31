(define mk-member?
  (lambda (f)
	(lambda (a l)
	  (cond
		((null? l) #f)
		((eq? a (car l)) #t)
		(else (f a (cdr l)))))))

(define member?
  (lambda (a l)
	((Y mk-member?) a l)))

(define Y
  (lambda (mk)
	((lambda (x) (x x))
	 (lambda (x) (mk (lambda (y) ((x x) y)))))))
