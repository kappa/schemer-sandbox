(define revl
  (lambda (l)
	(revl_do l (lambda () '()))
	))

(define revl_do
  (lambda (l fun)
	(cond
	  ((null? l) (fun))
	  (else (revl_do (cdr l) (lambda () (cons (car l) (fun)))))
	  )))

;(a b)
;
;(b)
;
;(define fun_f
;  (lambda ()
;	(cons b (lambda ()
;			  (cons a (lambda ()
;						'()))))
;	))
;

;(append '(a b c) '(x y z)) = (append '(a) '(b c x y z)) = (cons a '(b c x y z))

(define append
  (lambda (l1 l2)
	(cond
		((null? l1) l2) 
		(else (cons (car l1) (append (cdr l1) l2))))))

(define revlist
  (lambda (l)
	(cond 
	   ((null? l) '())
	   (else (append (revlist (cdr l)) (list (car l)))))))

(define revl2
  (lambda (l)
	(cond
	  ((null? l) '())
	  (else (revl2_do l '())))))

; append l2 to reverse of l
(define revl2_do
  (lambda (l l2)
	(cond
	  ((null? (cdr l)) (cons (car l) l2))
	  (else (revl2_do (cdr l) (cons (car l) l2))))))
