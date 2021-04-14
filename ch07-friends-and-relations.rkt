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

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a)
           (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

; p.111

(check-expect
 (set? '(apple peaches apple plum))
 #false)

(check-expect
 (let ((lat '(apples peaches pears plums)))
   (set? lat))
 #true)

(check-expect
 (let ((lat '()))
   (set? lat))
 #true)

(define set?
  (lambda (lat)
    (cond [(null? lat) #true]
          [(member? (car lat) (cdr lat))
           #false]
          [else (set? (cdr lat))])))

(check-expect
 (let ((lat '(3 pear 4 9 apple 3 4)))
   (set? lat))
 #false)

; p.112

(check-expect
 (let ((lat '(apple peach pear peach
                    plum apple lemon peach)))
   (make-set lat))
 '(pear plum apple lemon peach))

(define make-set
  (lambda (lat)
    (cond [(null? lat) '()]
          [(member? (car lat) (cdr lat))
           (make-set (cdr lat))]
          [else (cons (car lat)
                      (make-set (cdr lat)))])))

(define make-set2
  (lambda (lat)
    (cond [(null? lat) '()]
          [else
           (cons (car lat)
                 (make-set2
                  (multirember (car lat)
                               (cdr lat))))])))

(check-expect
 (let ((lat '(apple peach pear peach
                    plum apple lemon peach)))
   (make-set2 lat))
 '(apple peach pear plum lemon))

; p.113

(check-expect
 (let ((lat '(apple 3 pear 4 9 apple 3 4)))
   (make-set2 lat))
 '(apple 3 pear 4 9))

(check-expect
 (let ((set1 '(5 chicken wings))
       (set2 '(5 hamburgers
                 2 pieces fried chicken and
                 light duckling wings)))
   (subset? set1 set2))
 #true)

(check-expect
 (let ((set1 '(4 pounds of horseradish))
       (set2 '(four pounds chicken and
                    5 ounces horseradish)))
   (subset? set1 set2))
 #false)

(define subset?
  (lambda (set1 set2)
    (cond [(null? set1) #true]
          [else
           (and (member? (car set1) set2)
                (subset? (cdr set1) set2))])))

; p.114

(check-expect
 (let ((set1 '(6 large chickens with wings))
       (set2 '(6 chickens with large wings)))
   (eqset? set1 set2))
 #true)

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

; p.115

(check-expect
 (let ((set1 '(stewed tomatoes and macaroni))
       (set2 '(macaroni and cheese)))
   (intersect? set1 set2))
 #true)

(define intersect?
  (lambda (set1 set2)
    (cond [(null? set1) #false]
          [else (or (member? (car set1) set2)
                    (intersect? (cdr set1) set2))])))

; p.116

(check-expect
 (let ((set1 '(stewed tomatoes and macaroni))
       (set2 '(macaroni and cheese))))
 '(and macaroni))

(test)

