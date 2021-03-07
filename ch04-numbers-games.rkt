;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch01-toys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)

(define (atom? x)
  (not (list? x)))

;; Only numeric "primitives" we're given are `add1`, `sub1`, and `number?`,
;; everything else is user-defined.

; p.59

(check-expect
 (let ((n 14))
   (atom? 14))
 #true)

(check-expect
 (number? -3)
 #true)

(check-expect
 (number? -3.14159)
 #true)

(check-expect
 (let ((n 67))
   (add1 n))
 68)

(check-expect
 (let ((n 5))
   (sub1 n))
 4)

; p.60

;; Note: We're only considering negative numbers

(check-expect
 (zero? 0)
 #true)

(check-expect
 (zero? 1492)
 #false)

(check-expect
 (my+ 46 12)
 58)

(define my+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (my+ n (sub1 m)))))))

; p.61


(check-expect (my- 14 3) 11)
(check-expect (my- 17 9) 8)
(check-expect (- 18 25) -7)

(define my-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (my- n (sub1 m)))))))

; p.62

(check-expect
 (let ((tup '(3 5 2 8)))
   (addtup tup))
 18)

(check-expect
 (let ((tup '(15 6 7 12 3)))
   (addtup tup))
 43)

; p.63
; p.64

;; The First Commandment (first revision)
;; When recurring on a list of atoms, lat, ask two questions about it: `(null? lat)` and `else`.
;; When recurring on a number, n, ask two questions about it: `(zero? n)` and `else`.

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (my+ (car tup) (addtup (cdr tup)))))))

(check-expect
 (* 5 3)
 15)

(check-expect
 (* 13 4)
 52)

; p.65

;; The Fourth Commandment (first revision)
;; Always change at least one argument while recurring.
;; It must be changed to be closer to termination.
;; The changing argument must be tested in the termination condition:
;;  when using `cdr`, test termination with `null?` and
;;  when using `sub1`, test termination with `zero?`.

(check-expect
 (my* 5 3)
 15)

(check-expect
 (my* 13 4)
 52)

(define my*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (my+ n (my* n (sub1 m)))))))

; p.66
; p.67

;; The Fifth Commandment
;; When building a value with `my+`, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.
;; When building a value with `my*`, always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.
;; When building a value with `cons`, always consider () for the value of the terminating line.

(check-expect
 (let ((tup1 '(3 6 9 11 4))
       (tup2 '(8 5 2 0  7)))
   (tup+ tup1 tup2))
 '(11 11 11 11 11))

; p.68

(check-expect
 (let ((tup1 '(2 3))
       (tup2 '(4 6)))
   (tup+ tup1 tup2))
 '(6 9))

; p.69

(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (my+ (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))))))

(check-expect
 (let ((tup1 '(3 7))
       (tup2 '(4 6)))
   (tup+ tup1 tup2))
 '(7 13))

; p.70

(check-expect
 (let ((tup1 '(3 7))
       (tup2 '(4 6 8 1)))
   (tup+ tup1 tup2))
 '(7 13 8 1))

(check-expect
 (let ((tup1 '(3 7 8 1))
       (tup2 '(4 6)))
   (tup+ tup1 tup2))
 '(7 13 8 1))

; p.71

(check-expect
 (> 12 133)
 #false)

(check-expect
 (> 120 11)
 #true)

(check-expect
 (> 3 3)
 #false)

; p.72

(check-expect
 (my> 12 133)
 #false)

(check-expect
 (my> 120 11)
 #true)

(check-expect
 (my> 3 3)
 #false)

(define my>
  (lambda (n m)
    (cond ((zero? n) #false)
          ((zero? m) #true)
          (else (my> (sub1 n) (sub1 m))))))

; p.73

(check-expect
 (< 4 6)
 #true)

(check-expect
 (< 8 3)
 #false)

(check-expect
 (< 6 6)
 #false)

(check-expect
 (my< 4 6)
 #true)

(check-expect
 (my< 8 3)
 #false)

(check-expect
 (my< 6 6)
 #false)

(define my<
  (lambda (n m)
    (cond ((zero? m) #false)
          ((zero? n) #true)
          (else (my< (sub1 n) (sub1 m))))))

; p.74

(define tls=
  (lambda (n m)
    (cond ((zero? m) (zero? n))
          ((zero? n) #false)
          (else (tls= (sub1 n) (sub1 m))))))

(define my=
  (lambda (n m)
    (cond ((my> n m) #false)
          ((my< n m) #false)
          (else #true))))

(check-expect
 (up 1 1)
 1)

(check-expect
 (up 2 3)
 8)

(check-expect
 (up 5 3)
 125)

(define up
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (my* n (up n (sub1 m)))))))

(define my/
  (lambda (n m)
    (cond ((my< n m) 0)
          (else (add1 (my/ (my- n m) m))))))

;; What is a good name for this function? Modulo

; p.75
; p.76

(check-expect
 (let ((lat '(hotdogs with mustard sauerkraut and pickles)))
   (length lat))
 6)

(check-expect
 (let ((lat '(ham and cheese on rye)))
   (length lat))
 5)

(check-expect
 (let ((lat '(hotdogs with mustard sauerkraut and pickles)))
   (mylength lat))
 6)

(check-expect
 (let ((lat '(ham and cheese on rye)))
   (mylength lat))
 5)

(define mylength
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (add1 (mylength (cdr lat)))))))

(check-expect
 (let ((n 4)
       (lat '(lasagna spaghetti ravioli macaroni meatball)))
   (pick n lat))
 'macaroni)

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

; p.77

(check-expect
 (let ((n 3)
       (lat '(hotdogs with hot mustard)))
     (rempick n lat))
 '(hotdogs with mustard))

;; Removes element at position in list
;; (define rempick
;;   (lambda (n lat)
;;     (cond ((zero? (sub1 n)) (cdr lat))
;;           (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(check-expect
 (number? 'tomato)
 #false)

(check-expect
 (number? 76)
 #true)

(check-expect
 (let ((lat '(5 pears 6 prunes 9 dates)))
   (no-nums lat))
 '(pears prunes dates))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat) (no-nums (cdr lat)))))))

; p.78

(check-expect
 (let ((lat '(5 pears 6 prunes 9 dates)))
   (all-nums lat))
 '(5 6 9))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat))
           (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

;; Write the function eqan? which is true if its
;; two arguments (al and a2) are the same
;; atom. Remember to use = for numbers and
;; eq? for all other atoms.

(check-expect (eqan? 1 1) #true)
(check-expect (eqan? 'a 'a) #true)
(check-expect (eqan? 1 2) #false)
(check-expect (eqan? 'a 'b) #false)
(check-expect (eqan? 'a 2) #false)
(check-expect (eqan? 1 'b) #false)

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
           (my= a1 a2))
          ((or  (number? a1)  (number? a2))
           #false)
          (else (eq? a1 a2)))))

;; Now write the function occur which counts
;; the number of times an atom a appears in a
;; lat

(check-expect (occur 'a '(a 1 b 2 a 4 c a)) 3)
(check-expect (occur 3 '(a 3 b 2 a 3 c a)) 2)


(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

; p.79

;; Write the function one? where (one? n) is #true
;; if n is 1 and #f (Le., false) otherwise.

(check-expect (one? 3) #false)
(check-expect (one? 1) #true)
(check-expect (one? 'a) #false)

(define one?
  (lambda (n)
    (eqan? 1 n)))

;; rewrite rempick using `one?`

(check-expect
 (let ((n 3)
       (lat '(lemon meringue salty pie)))
     (rempick n lat))
 '(lemon meringue pie))

(define rempick
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
          (else (cons (car lat)
                      (rempick (sub1 n)
                               (cdr lat)))))))

(test)
