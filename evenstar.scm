(define atom?
  (lambda (a) 
	(not (or (null? a) (pair? a)))))

(define even-only*
  (lambda (l)
	(cond
	  ((null? l) '())
	  ((atom? (car l))
	   (cond
		 ((even? (car l)) (cons (car l) (even-only* (cdr l))))
		 (else (even-only* (cdr l)))))
	  (else (cons (even-only* (car l)) (even-only* (cdr l)))))))

; collection function takes the new list, sum of odds and product of
; evens
(define evens-only*&co
  (lambda (l f)
	(cond
	  ((null? l) (f '() 0 1))
	  ((atom? (car l))
	   (cond
		 ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl sum_o prod_e)
													(f (cons (car l) newl) sum_o (* (car l) prod_e)))))
		 (else (evens-only*&co (cdr l) (lambda (newl sum_o prod_e)
													   (f newl (+ (car l) sum_o) prod_e))))))
	  (else (evens-only*&co (car l) 
							(lambda (newl sum_o prod_e)
							  (evens-only*&co (cdr l)
											  (lambda (newl_cdr sum_o_cdr prod_e_cdr)
												(f (cons newl newl_cdr)
												   (+ sum_o sum_o_cdr)
												   (* prod_e prod_e_cdr))))))))))
