(define multiinsertL
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
	  (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
	(cond
	  ((null? lat) '())
	  ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
	  (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
	(cond
	  ((null? lat) '())
	  ((eq? (car lat) oldR) (cons oldR (cons new  (multiinsertLR new oldL oldR (cdr lat)))))
	  ((eq? (car lat) oldL) (cons new  (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
	  (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define dump3
  (lambda (newlat cL cR)
	(list newlat cL cR)))

; cal f on the new lat, the number of left insertions and the number
; of right insertions
(define multiinsertLR_co
  (lambda (new oldL oldR lat f)
	(cond
	  ((null? lat) (f '() 0 0))
	  ((eq? (car lat) oldL)
	   (multiinsertLR_co new oldL oldR (cdr lat)
											   (lambda (l cL cR)
												 (f (cons new (cons oldL l)) (+ 1 cL) cR))))
	  ((eq? (car lat) oldR)
	   (multiinsertLR_co new oldL oldR (cdr lat)
											   (lambda (l cL cR)
												 (f (cons oldR (cons new l)) cL (+ 1 cR)))))
	  (else (multiinsertLR_co new oldL oldR (cdr lat)
											   (lambda (l cL cR)
												 (f (cons (car lat) l) cL cR)))))))
