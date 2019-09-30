;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch01-toys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)

(define (atom? x)
  (not (list? x)))

(define (sexp? x)
  (or (atom? x) (list? x)))

;; The Law of Car: The primitive car is defined only for non-empty lists.
;; The Law of Cdr: The primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is always another list.
;; The Law of Cons: The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.
;; The Law of Null?: The primitive null? is defined only for lists
;; The Law of Eq?: The primitive eq? takes two arguments. Each must be a non-numeric atom.

;; (define lat?
;;   (lambda (l)
;;     (cond
;;       ((null? l) #true)
;;       ((atom? (car l)) (lat? (cdr l)))
;;       (else #false))))

(define (lat? l)
  (cond [(null? l) #true]
        [(atom? (car l)) (lat? (cdr l))]
        [else #false]))

; p.15

(check-expect
 (let ([l '(Jack Sprat could eat no chicken fat)])
   (lat? l))
 #true)

(check-expect
 (let ([l '((Jack) Sprat could eat no chicken fat)])
   (lat? l))
 #false)

(check-expect
 (let ([l '(Jack (Sprat could) eat no chicken fat)])
   (lat? l))
 #false)

(check-expect
 (let ([l '()])
   (lat? l))
 #true)

;; True or false: a lat is a list of atoms
;; -> True

(define (my-lat? l)
  (cond [(eq? l '()) #true]
        [(not (atom? (car l))) #false]
        [else (my-lat? (cdr l))]))

; p.16

(check-expect
 (let ([l '(bacon and eggs)])
   (lat? l))
 #true)

; p.17
; p.18
; p.19

(check-expect
 (let ([l '(bacon (and eggs))])
   (lat? l))
 #false)

; p.20
; p.21

(check-expect
 (let ([l1 '()]
       [l2 '(d e f g)])
   (or (null? l1)
       (atom? l2)))
 #true)

(check-expect
 (let ([l1 '(a b c)]
       [l2 '()])
   (or (null? l1)
       (null? l2)))
 #true)

(check-expect
 (let ([l1 '(a b c)]
       [l2 '(atom)])
   (or (null? l1) (null? l2)))
 #false)

; p.22

(define tls-member?
  (lambda (a lat)
    (cond
      ((null? lat) #false)
      (else (or (eq? (car lat) a)
                (tls-member? a (cdr lat)))))))

(check-expect
 (let ([a 'tea]
       [lat '(coffee tea or milk)])
   (tls-member? a lat))
 #true)

(check-expect
 (let ([a 'poached]
       [lat '(fried eggs and scrambled eggs)])
   (tls-member? a lat))
 #false)

(check-expect
 (let ([a 'meat]
       [lat '(mashed potatoes and meat gravy)])
   (tls-member? a lat))
 #true)

;; The First Commandment: (preliminary) Always ask null? as the first question in expressing any function.

; p.23

(check-expect
 (let ([a 'meat]
       [lat '(mashed potatoes and meat gravy)])
   (or (eq? (car lat) a)
       (tls-member? a (cdr lat))))
 #true)

; p.24

(check-expect
 (let ([a 'meat]
       [lat '(mashed potatoes and meat gravy)])
   (eq? (car lat) a))
 #false)

(check-expect
 (let ([lat '(potatoes and meat gravy)])
   (null? lat))
 #false)

; p.26

(check-expect
 (let ([a 'meat]
       [lat '(meat gravy)])
   (or (eq? (car lat) a)
       (tls-member? a (cdr lat))))
 #true)

(check-expect
 (let ([a 'meat]
       [lat '(meat gravy)])
   (tls-member? a lat))
 #true)

(check-expect
 (let ([a 'meat]
       [lat '(and meat gravy)])
   (tls-member? a lat))
 #true)

(check-expect
 (let ([a 'meat]
       [lat '(potatoes and meat gravy)])
   (tls-member? a lat))
 #true)

(check-expect
 (let ([a 'meat]
       [lat '(mashed potatoes and meat gravy)])
   (tls-member? a lat))
 #true)

; p.28

(check-expect
 (let ((a 'meat)
       (lat '(meat gravy)))
   (tls-member? a lat))
 #true)

(check-expect
 (let ((a 'meat)
       (lat '(and meat gravy)))
   (tls-member? a lat))
 #true)

(check-expect
 (let ((a 'meat)
       (lat '(potatoes and meat gravy)))
   (tls-member? a lat))
 #true)

(check-expect
 (let ((a 'meat)
       (lat '(mashed potatoes and meat gravy)))
   (tls-member? a lat))
 #true)

(check-expect
 (let ([a 'liver]
       [lat '(bagels and lox)])
   (tls-member? a lat))
 #false)

(check-expect
 (let ([a 'liver]
       [lat '(lox)])
   (tls-member? a lat))
 #false)

(check-expect
 (let ([a 'liver]
       [lat '(and lox)])
   (tls-member? a lat))
 #false)

(check-expect
 (let ([a 'liver]
       [lat '(and lox)])
   (or (eq? (car lat) a)
       (tls-member? a (cdr lat))))
 #false)

(check-expect
 (let ([a 'liver]
       [lat '(bagels and lox)])
   (tls-member? a lat))
 #false)
 
(test)

