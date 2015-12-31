(define identity
  (lambda (a)
	a))

(define identity2
  (lambda (a b)
	(list a b)))

(define sum
  (lambda (l)
	(cond
	  ((null? l) 0)
	  (else (+ (car l) (sum (cdr l)))))))

(define sum_co
  (lambda (l f)
	(cond
	  ((null? l) (f 0))
	  (else (sum_co (cdr l) (lambda (a)
							  (f (+ (car l) a))))))))

(define sum&prod_co
  (lambda (l f)
	(cond
	  ((null? l) (f 0 1))
	  (else (sum&prod_co (cdr l) (lambda (sum prod)
								   (f (+ (car l) sum) (* (car l) prod))))))))

; naive implementation
; we recur twice so it's ineffective unless memoized 
(define sum&prod
  (lambda (l)
	(cond
	  ((null? l) '(0 1))
	  (else (list (+ (car l) (car (sum&prod (cdr l))))
				  (* (car l) (car (cdr (sum&prod (cdr l))))))))))

; return a pair of sum and product
(define sum&prod_iter
  (lambda (a l2)
	(list (+ a (car l2)) (* a (cadr l2)))))

; less naive implementation
(define sum&prod_o
  (lambda (l)
	(cond
	  ((null? l) '(0 1))
	  (else (sum&prod_iter (car l) (sum&prod_o (cdr l)))))))

