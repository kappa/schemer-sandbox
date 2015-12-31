(define add1
  (lambda (a)
	(+ a 1)))
(define sub1 
  (lambda (a)
	(- a 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o+ (add1 n) (sub1 m))))))
(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))

(define addtup
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons (o+ (car tup1) (car tup2)) (addtup (cdr tup1) (cdr tup2)))))))

(addtup '(1 2 10) '(1 1 1))
