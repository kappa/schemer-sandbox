(define eternity
  (lambda (l)
	(eternity l)))

(define add1
  (lambda (n)
	(+ n 1)))

(lambda (l)
  (cond
	((null? l) 0)
	(else (add1 (eternity (cdr l))))))

(lambda (l)
  (cond
	((null? l) 0)
	(else
	  (add1 ((lambda (l)
				(cond
					((null? l) 0)
					(else
					  (add1 (eternity (cdr l)))))) (cdr l))))))

((lambda (length)
  (lambda (l)
	  (cond
		((null? l) 0)
		(else (add1 (length (cdr l)))))))
 eternity)

((lambda (len)
  (lambda (l)
	  (cond
		((null? l) 0)
		(else (add1 (len (cdr l)))))))
 ((lambda (len)
	(lambda (l)
	  (cond
		((null? l) 0)
		(else (add1 (len (cdr l)))))))
  ((lambda (len)
	(lambda (l)
	  (cond
		((null? l) 0)
		(else (add1 (len (cdr l)))))))
   eternity)))

(((lambda (mk-length)
  (mk-length
	(mk-length
	  (mk-length
		(mk-length
		  (mk-length
			(mk-length eternity))))))) 
 (lambda (len)
  (lambda (l)
   (cond
	 ((null? l) 0)
	 (else (add1 (len (cdr l)))))))) '(a b c d e))

(((lambda (mk-length)
  (mk-length mk-length)) 
 (lambda (mk-length)
  (lambda (l)
   (cond
	 ((null? l) 0)
	 (else (add1
			 ((mk-length mk-length)
			  (cdr l)))))))) '(a b c d e f g))

(((lambda (mk-length)
  (mk-length mk-length)) 
 (lambda (mk-length)
  ((lambda (lenn)
	 (lambda (l)
	   (cond
		 ((null? l) 0)
		 (else (add1
				 (lenn (cdr l)))))))
   (lambda (x) ((mk-length mk-length) x)))))
 '(a b c))

(define len-y
  (Y (lambda (ly)
	   (lambda (l)
		 (cond
		   ((null? l) 0)
		   (else (add1 (ly (cdr l)))))))))

(define fact
  (Y (lambda (f)
	   (lambda (n)
		 (cond
		   ((zero? n) 1)
		   (else (* n (f (- n 1)))))))))

(define Y
  (lambda (f)
	((lambda (x) (x x))
	 (lambda (x) (f (lambda (y) ((x x) y)))))))

(define (Y f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (y) ((x x) y))))))

(define member?
  (Y (lambda (f)
	   (lambda (a l)
		 (cond
		   ((null? l) #f)
		   ((eq? a (car l)) #t)
		   (else (f a (cdr l))))))))
