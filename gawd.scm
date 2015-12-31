;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gawd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) a) (rember* a (cdr l)))
                         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? (car l) old)
                          (cons (car l) (cons new (insertR* new old (cdr l)))))
                         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (add1 (occur* a (cdr l))))
                         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l))
                          (cons new (cons (car l) (insertL* new old (cdr l)))))
                         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
                       