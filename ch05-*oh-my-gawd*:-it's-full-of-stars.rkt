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

(define my+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (my+ n (sub1 m)))))))

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

; p.81

(check-expect
 (let ((a 'cup)
       (l '((coffee) cup ((tea) cup) (and (hick)) cup)))
   (rember* a l))
 '((coffee) ((tea)) (and (hick))))

(check-expect
 (let ((a 'sauce)
       (l '(((tomato sauce))
            ((bean) sauce)
            (and ((flying)) sauce))))
   (rember* a l))
 '(((tomato)) ((bean)) (and ((flying)))))

(define rember*
  (lambda (a l)
    (cond [(null? l) '()]
          [(atom? (car l))
           [cond [(eq? (car l) a) (rember* a (cdr l))]
                 [else (cons (car l) (rember* a (cdr l)))]]]
          [else (cons (rember* a (car l))
                      (rember* a (cdr l)))])))

(check-expect
 (let ((l '(((tomato sauce))
            ((bean) sauce)
            (and ((flying)) sauce))))
   (lat? l))
 #false)

; p.82

(check-expect
 (let ((l '(((tomato sauce))
            ((bean) sauce)
            (and ((flying)) sauce))))
   (atom? (car l)))
 #false)

(check-expect
 (let ((new 'roast)
       (old 'chuck)
       (l '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood)))
   (insertR* new old l))
 '((how much (wood))
   could
   ((a (wood) chuck roast))
   (((chuck roast)))
   (if (a) ((wood chuck roast)))
   could chuck roast wood))

(define insertR*
  (lambda (new old l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? (car l) old)
                  (cons old (cons new (insertR* new old (cdr l))))]
                 [else
                  (cons (car l) (insertR* new old (cdr l)))])]
          [else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))])))

; p.83

;; The First Commandment (final version).
;; When recurring on a list of atoms, lat, ask two questions about it:
;;   `(null? lat)` and `else`.
;; When recurring on a list of S-expressions, l, ask three questions about it:
;;   `(null? l)`, `(atom? (car l))`, and `else`.

; p.84

;; The Fourth Commandment (final version).

;; Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use `(cdr lat)`.
;; When recurring on a number, n, use `(sub1 n)`.
;; And when recurring on a list of S-expressions, l, use `(car l)` and `(cdr l)` if neither `(null?)` nor `(atom? (car l))` are true.

;; It must be changed to be closer to termination.
;; The changing argument must be tested in the termination condition:
;;  when using `cdr`, test termination with `null?` and
;;  when using `sub1`, test termination with `zero?`.

(check-expect
 (let ((a 'banana)
       (l '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy))))
   (occur* a l))
 5)

; p.85

; counts number of occurrences
(define occur*
  (lambda (a l)
    (cond [(null? l) 0]
          [(atom? (car l))
           (cond [(eq? (car l) a)
                  (add1 (occur* a (cdr l)))]
                 [else (occur* a (cdr l))])]
          [else
           (my+ (occur* a (car l))
                (occur* a (cdr l)))])))

(check-expect
 (let ((new 'orange)
       (old 'banana)
       (l '((banana)
            (split ((((banana ice)))
                    (cream (banana))
                    sherbet))
            (banana)
            (bread)
            (banana brandy))))
   (subst* new old l))
 '((orange)
   (split ((((orange ice)))
           (cream (orange))
           sherbet))
   (orange)
   (bread)
   (orange brandy)))

(define subst*
  (lambda (new old l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? (car l) old) (cons new (subst* new old (cdr l)))]
                 [else (cons (car l) (subst* new old (cdr l)))])]
          [else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))])))

; p.86

(check-expect
 (let ((new 'pecker)
       (old 'chuck)
       (l '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood)))
   (insertL* new old l))
 '((how much (wood))
   could
   ((a (wood) pecker chuck))
   (((pecker chuck)))
   (if (a) ((wood pecker chuck)))
   could pecker chuck wood))

(define insertL*
  (lambda (new old l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(eq? (car l) old)
                  (cons new (cons old (insertL* new old (cdr l))))]
                 [else (cons (car l) (insertL* new old (cdr l)))])]
          [else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))])))

(check-expect
 (let ((a 'chips)
       (l '((potato) (chips ((with) fish) (chips)))))
   (member* a l))
 #true)

; p.87

(define member*
  (lambda (a l)
    (cond [(null? l) #false]
          [(atom? (car l))
           (cond [(eq? (car l) a) #true]
                 [else (member* a (cdr l))])]
          [else (or (member* a (car l))
                    (member* a (cdr l)))])))

(check-expect
 (let ((l '((potato) (chips ((with) fish) (chips)))))
   (leftmost l))
 'potato)

(check-expect
 (let ((l '(((hot) (tuna (and))) cheese)))
   (leftmost l))
 'hot)

(check-error
 (leftmost '(((() four) 17 (seventeen)))))

(check-error
 (leftmost '()))

; p.88

(define leftmost
  (lambda (l)
    (cond [(atom? (car l)) (car l)]
          [else (leftmost (car l))])))

(check-expect
 (let ((x 'pizza)
       (l '(mozzarella pizza)))
   (and (atom? (car l))
        (eq? (car l) x)))
 #false)

; p.89

(check-expect
 (let ((x 'pizza)
       (l '((mozzarella mushroom) pizza)))
   (and (atom? (car l))
        (eq? (car l) x)))
 #false)

(check-expect
 (let ((x 'pizza)
       (l '(pizza pie)))
   (and (atom? (car l))
        (eq? (car l) x)))
 #true)

(check-expect
 (let ((l1 '(strawberry ice cream))
       (l2 '(strawberry ice cream)))
   (eqlist? l1 l2))
 #true)

; p.90

(check-expect
 (let ((l1 '(strawberry ice cream))
       (l2 '(strawberry cream ice)))
   (eqlist? l1 l2))
 #false)

(check-expect
 (let ((l1 '(banana ((split))))
       (l2 '((banana) (split))))
   (eqlist? l1 l2))
 #false)

(check-expect
 (let ((l1 '(beef ((sausage)) (and (soda))))
       (l2 '(beef ((salami)) (and (soda)))))
   (eqlist? l1 l2))
 #false)

(check-expect
 (let ((l1 '(beef ((sausage)) (and (soda))))
       (l2 '(beef ((sausage)) (and (soda)))))
   (eqlist? l1 l2))
 #true)

; p.91

;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;       ;; l1: empty, l2: empty
;;       [(and (null? l1) (null? l2)) #true]
;;       ;; l1: empty, l2: atom
;;       [(and (null? l1) (atom? (car l2))) #false]
;;       ;; l1: empty, l2: list
;;       [(null? l1) #false]
;;       ;; l1: atom, l2: empty
;;       [(and (atom? (car l1)) (null? l2)) #false]
;;       ;; l1: atom, l2: atom                             <- recurse when same-same
;;       [(and (atom? (car l1)) (atom? (car l2)))
;;        (and (eqan? (car l1) (car l2))
;;             (eqlist? (cdr l1) (cdr l2)))]
;;       ;; l1: atom, l2: list
;;       [(atom? (car l1)) #false]
;;       ;; l1: list, l2: empty
;;       [(null? l2) #false]
;;       ;; l1: list, l2: atom
;;       [(atom? (car l2)) #false]
;;       ;; l1: list, l2: list                            <- recurse when same-same
;;       [else
;;        (and (eqlist? (car l1) (car l2))
;;             (eqlist? (cdr l1) (cdr l2)))])))

; p.92

;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;       [(and (null? l1) (null? l2)) #true]
;;       [(or (null? l1) (null? l2)) #false]
;;       [(and (atom? (car l1)) (atom? (car l2)))
;;        (and (eqan? (car l1) (car l2))
;;             (eqlist? (cdr l1) (cdr l2)))]
;;       [(or (atom? (car l1)) (atom? (car l2))) #false]
;;       [else
;;        (and (eqlist? (car l1) (car l2))
;;             (eqlist? (cdr l1) (cdr l2)))])))

; p.93

(define myequal?
  (lambda (s1 s2)
    (cond [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
          [(or (atom? s1) (atom? s2)) #false]
          [else (eqlist? s1 s2)])))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #true]
      [(or (null? l1) (null? l2)) #false]
      [else (and (myequal? (car l1) (car l2))
                 (myequal? (cdr l1) (cdr l2)))])))

; p.94

;; The Sixth Commandment
;; Simplify only after the function is correct.

(check-expect
 (rember 'a '(f e a b c d))
 '(f e b c d))

(check-expect
 (rember '(a b) '((a e) (a b) (a f)))
 '((a e) (a f)))

; p.95

(define rember
  (lambda (s l)
    (cond ((null? l) (quote ()))
          ((myequal? (car l) s) (cdr l))
          (else (cons (car l) (rember s (cdr l)))))))

(test)

