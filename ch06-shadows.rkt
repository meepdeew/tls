;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch01-toys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)

(define (atom? x)
  (not (list? x)))

(define (sexp? x)
  (or (atom? x) (list? x)))

(define lat?
  (lambda (l)
    (cond
      [(null? l) #true]
      [(atom? (car l)) (lat? (cdr l))]
      [else #false])))

(define my*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (my+ n (my* n (sub1 m)))))))

(define my+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (my+ n (sub1 m)))))))

(define up
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (my* n (up n (sub1 m)))))))

(define my<
  (lambda (n m)
    (cond ((zero? m) #false)
          ((zero? n) #true)
          (else (my< (sub1 n) (sub1 m))))))

(define my>
  (lambda (n m)
    (cond ((zero? n) #false)
          ((zero? m) #true)
          (else (my> (sub1 n) (sub1 m))))))

(define my=
  (lambda (n m)
    (cond ((my> n m) #false)
          ((my< n m) #false)
          (else #true))))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (my= a1 a2))
          ((or  (number? a1)  (number? a2))
           #false)
          (else (eq? a1 a2)))))

; p.97

(check-expect
 (let ((y 'a))
   (eq? (quote a) y))
 #true)

(check-expect
 (let ((x 'a)
       (y 'a))
   (eq? x y))
 #true)

; p.98

(check-expect
 (let ((x 1))
   (numbered? x))
 #true)

(check-expect
 (let ((y '(3 my+ (4 up 5))))
   (numbered? y))
 #true)

(check-expect
 (let ((z '(2 my* sausage)))
   (numbered? z))
 #false)

; p.99

;; (define numbered?
;;   (lambda (aexp)
;;     (cond [(null? aexp) #true]
;;           [(atom? aexp)
;;            (cond [(number? aexp) #true]
;;                  [(eq? aexp 'my+) #true]
;;                  [(eq? aexp 'my*) #true]
;;                  [(eq? aexp 'up) #true]
;;                  [else #false])]
;;           [else
;;            (and (numbered? (car aexp))
;;                 (numbered? (cdr aexp)))])))
;; ;; Note: ^ wrong, because we're assuming well-formed arithmetic input..

; p.100
; p.101

(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))])))

(check-expect
 (let ((u 13))
   (value u))
 13)

(check-expect
 (let ((x '(1 my+ 3)))
   (value x))
 4)

; p.102

(check-expect
 (let ((y '(1 my+ (3 up 4))))
   (value y))
 82)

(define value
  (lambda (nexp)
    (cond [(atom? nexp) nexp]
          [(eq? (car (cdr nexp)) 'my+)
           (my+ (value (car nexp))
                (value (car (cdr (cdr nexp)))))]
          [(eq? (car (cdr nexp)) 'my*)
           (my* (value (car nexp))
                (value (car (cdr (cdr nexp)))))]
          [(eq? (car (cdr nexp)) 'up)
           (up (value (car nexp))
               (value (car (cdr (cdr nexp)))))])))

; p.103

;; The Seventh Commandment
;; Recur on the subparts that are of the same nature:
;; - On the sublists of a list.
;; - On the subexpressions of an arithmetic expression.

; p.104

;; (define prefix-value
;;   (lambda (nexp)
;;     (cond [(atom? nexp) nexp]
;;           [(eq? (car nexp) 'my+)
;;            (my+ (prefix-value (cdr nexp))
;;                 (prefix-value (cdr (cdr nexp))))]
;;           [(eq? (car nexp) 'my*)
;;            (my* (prefix-value (cdr nexp))
;;                 (prefix-value (cdr (cdr nexp))))]
;;           [else
;;            (up (prefix-value (cdr nexp))
;;                (prefix-value (cdr (cdr nexp))))])))

; p.105

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; p.106

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define prefix-value
  (lambda (nexp)
    (cond [(atom? nexp) nexp]
          [(eq? (operator nexp) 'my+)
           (my+ (prefix-value (1st-sub-exp nexp))
                (prefix-value (2nd-sub-exp nexp)))]
          [(eq? (operator nexp) 'my*)
           (my* (prefix-value (1st-sub-exp nexp))
                (prefix-value (2nd-sub-exp nexp)))]
          [else
           (up (prefix-value (1st-sub-exp nexp))
               (prefix-value (2nd-sub-exp nexp)))])))

; p.107

;; The Eighth Commandment
;; Use help functions to abstract from representations

; p.108

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define new+
  (lambda (n m)
    (cond [(sero? m) n]
          [else (edd1 (new+ n (zub1 m)))])))

; p.109

(test)

