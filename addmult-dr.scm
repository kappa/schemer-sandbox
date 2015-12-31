;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname addmult-dr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o+ (add1 n) (sub1 m))))))
(define ×
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (× n (sub1 m)))))))

(define addtup
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (addtup (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (× n (^ n (sub1 m)))))))