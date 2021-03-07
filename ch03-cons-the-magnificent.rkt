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
      ((null? l) #true)
      ((atom? (car l)) (lat? (cdr l))))))

(define tls-member?
  (lambda (a lat)
    (cond
      ((null? lat) #false)
      (else (or (eq? (car lat) a)
                (tls-member? a (cdr lat)))))))

;; The Law of Car: The primitive car is defined only for non-empty lists.
;; The Law of Cdr: The primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is always another list.
;; The Law of Cons: The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.
;; The Law of Null?: The primitive null? is defined only for lists
;; The Law of Eq?: The primitive eq? takes two arguments. Each must be a non-numeric atom.

;; The First Commandment: (preliminary) Always ask null? as the first question in expressing any function.

; p.33

(check-expect
 (let ((a 'mint)
       (lat '(lamb chops and mint jelly)))
   (rember a lat))
 '(lamb chops and jelly))


(check-expect
 (let ((a 'mint)
       (lat '(lam chops and mint flavored mint jelly)))
   (rember a lat))
 '(lam chops and flavored mint jelly))

(check-expect
 (let ((a 'toast)
       (lat '(bacon lettuce and tomato)))
   (rember a lat))
 '(bacon lettuce and tomato))

(check-expect
 (let ((a 'cup)
       (lat '(coffee cup tea cup and hick cup)))
   (rember a lat))
 '(coffee tea cup and hick cup))

; p.34
(define rember
  (lambda (a lat)
    (cond ((null? lat) (quote ()))
          ((eq? (car lat) a) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))

(check-expect
 (let ((a 'bacon)
       (lat '(bacon lettuce and tomato)))
   (rember a lat))
 '(lettuce and tomato))

; p.35
(check-expect
 (let ((a 'and)
       (lat '(bacon lettuce and tomato)))
   (rember a lat))
 '(bacon lettuce tomato))

; p.36
; p.37

;; The Second Commandment: Use cons to build lists.

(check-expect
 (let ((a 'and)
       (lat '(bacon lettuce and tomato)))
   (rember a lat))
 '(bacon lettuce tomato))

; p.38
; p.39

(check-expect
 (cons 'lettuce '(tomato))
 '(lettuce tomato))

(check-expect
 '(lettuce tomato)
 (cons 'lettuce (cons 'tomato '())))

; p.40

(check-expect
 (cons 'bacon '(lettuce tomato))
 '(bacon lettuce tomato))

; p.41
; p.42

(check-expect
 (let ((a 'sauce)
       (lat '(soy sauce and tomato sauce)))
   (rember a lat))
 '(soy and tomato sauce))

; p.43

(define firsts
  (lambda (lat)
    (cond ((null? lat) '())
          (else (cons (car (car lat)) (firsts (cdr lat)))))))

(check-expect
 (let ((l '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant))))
   (firsts l))
 '(apple plum grape bean))

(check-expect
 (let ((l '((a b)
            (c d)
            (e f))))
   (firsts l))
 '(a c e))

(check-expect (firsts '())
              '())

(check-expect
 (let ((l '((five plums)
            (four)
            (eleven green oranges))))
   (firsts l))
 '(five four eleven))

(check-expect
 (let ((l '(((five plums) four)
            (eleven green oranges)
            ((no) more))))
   (firsts l))
 '((five plums) eleven (no)))

; p.44
; p.45

;; The Third Commandment. When building a list, describe the first typical element, and then cons it onto the natural recursion.

(define seconds
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (cdr (car l))) (seconds (cdr l)))))))
(check-expect
 (seconds '((a b) (c d) (e f)))
 '(b d f))

; p.46
; p.47

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(check-expect
 (let ((new 'topping)
       (old 'fudge)
       (lat '(ice cream with fudge for dessert)))
   (insertR  new old lat))
 '(ice cream with fudge topping for dessert))

(check-expect
 (let ((new 'jalapeno)
       (old 'and)
       (lat '(tacos tamales and salsa)))
   (insertR new old lat))
 '(tacos tamales and jalapeno salsa))

; p.48

(check-expect
 (insertR 'e 'd '(a b c d f g d h))
 '(a b c d e f g d h))

; p.49
; p.50

(check-expect
 (insertR 'topping 'fudge '(ice cream with fudge for dessert))
 '(ice cream with fudge topping for dessert))

; p.51

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new lat))
          (else (cons (car lat) (insertL new old (cdr lat)))))))

(check-expect
 (insertL 'green 'fudge '(ice cream with fudge for dessert))
 '(ice cream with green fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(check-expect
 (subst 'topping 'fudge '(ice cream with fudge for dessert))
 '(ice cream with topping for dessert))

; p.52

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
          ((or (eq? (car lat) o1)
               (eq? (car lat) o2))
           (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst2 new o1 o2 (cdr lat)))))))

(check-expect
 (subst2 'vanilla 'chocolate 'banana
         '(banana ice cream with chocolate topping))
 '(vanilla ice cream with chocolate topping))

; p.53

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a)
           (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(check-expect
 (multirember 'cup '(coffee cup tea cup and hick cup))
 '(coffee tea and hick))

; p.54
; p.55
; p.56

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(check-expect (multiinsertR 'd 'b '(a b c))
              '(a b d c))
(check-expect (multiinsertR 'd 'b '(a b c a b c))
              '(a b d c a b d c))

; p.57

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new (cons old (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(check-expect (multiinsertL 'd 'b '(a b c))
              '(a d b c))
(check-expect (multiinsertL 'd 'b '(a b c a b c))
              '(a d b c a d b c))

;; The Fourth Commandment (preliminary). Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition: when using cdr, test termination with `null?`.

(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))

(check-expect (multisubst 'd 'b '(a b c))
              '(a d c))
(check-expect (multisubst 'd 'b '(a b c a b c))
              '(a d c a d c))

(test)

