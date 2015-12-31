;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname insertr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define insertR
    (lambda (new old lat)
      (cond
        ((null? lat) lat)
        ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
    (lambda (new old lat)
      (cond
        ((null? lat) lat)
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
    (lambda (new old lat)
      (cond
        ((null? lat) lat)
        ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
    (lambda (new old lat)
      (cond
        ((null? lat) lat)
        ((eq? (car lat) old) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))