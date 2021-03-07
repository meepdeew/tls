;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ch01-toys) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)

(define (atom? x)
  (not (list? x)))

(define (sexp? x)
  (or (atom? x) (list? x)))

; p.3

(check-expect (atom? 'atom)
              #true)

(check-expect (atom? 'turkey)
              #true)

(check-expect (atom? 1492)
              #true)

(check-expect (atom? 'u)
              #true)

(check-expect (atom? '*abc$)
              #true)

(check-expect (list? '(atom))
              #true)

(check-expect (list? '(atom turkey or))
              #true)

; p.4

(check-error (list? '(atom turkey) 'or)
             "list?: expects only 1 argument, but found 2")

(check-expect (list? '((atom turkey) 'or))
              #true)

(check-expect (sexp? 'xyz)
              #true)

(check-expect (sexp? '(x y z))
              #true)

(check-expect (sexp? '((x y) z))
              #true)

(check-expect (list? '(how are you doing so far))
              #true)

(check-expect (length '(how are you doing so far))
              6)

(check-expect (list? '(((how) are) ((you) (doing so)) far))
              #true)

(check-expect (length '(((how) are) ((you) (doing so)) far))
              3)

; p.5

(check-expect (list? '())
              #true)

(check-expect (atom? '()) #false) ; no, because () is just a list.

(check-expect (list? '(() () () ()))
              #true)

(check-expect
 (let ([l '(a b c)])
   (car l)) 
 'a)

(check-expect
 (let ([l '((a b c) x y z)])
   (car l)) 
 '(a b c))

(check-error
 (let ([l 'hotdog])
   (car l)) 
 "car: expects a pair, given 'hotdog")

(check-error
 (let ([l '()])
   (car l))
 "car: expects a pair, given '()")

;; The Law of Car: The primitive car is defined only for non-empty lists.

; p.6

(check-expect
 (let ([l '(((hotdogs)) (and) (pickle) relish)])
   (car l)) 
 '((hotdogs)))

(check-expect
 (let ([l '(((hotdogs)) (and))])
   (car (car l)))
 '(hotdogs))

(check-expect
 (let ([l '(a b c)])
   (cdr l)) 
 '(b c))

(check-expect
 (let ([l '((a b c) x y z)])
   (cdr l))
 '(x y z))

(check-expect
 (let ([l '(hamburger)])
   (cdr l))
 '())

(check-expect
 (let ([l '((x) t r)])
   (cdr l))
 '(t r))

(check-error
 (let ([a 'hotdogs])
   (cdr a)) 
 "cdr: expects a pair, given 'hotdogs")

; p.7

(check-error
 (let ([l '()])
   (cdr l)) 
 "cdr: expects a pair, given '()")

;; The Law of Cdr: The primitive cdr is defined only for non-empty lists. The cdr of any non-empty list is always another list.

(check-expect
 (let ([l '((b) (x y) ((c)))])
   (car (cdr l)))
 '(x y))

(check-expect
 (let ([l '((b) (x y) ((c)))])
   (cdr (cdr l))) 
 '(((c))))

(check-error
 (let ([l '(a (b (c)) d)])
   (cdr (car l)))
 "cdr: expects a pair, given 'a")

;; What does car take as an argument?
;; -> a cons cell (a pair)

;; What does cdr take as an argument?
;; -> a cons cell (a pair)

(check-expect
 (let ([a 'peanut]
       [l '(butter and jelly)])
   (cons a l)) 
 '(peanut butter and jelly))

; p.8

(check-expect
 (let ([s '(banana and)]
       [l '(peanut butter and jelly)])
   (cons s l)) 
 '((banana and) peanut butter and jelly))

(check-expect
 (let ([s '((help) this)]
       [l '(is very ((hard) to learn))])
   (cons s l))
 '(((help) this) is very ((hard) to learn)))

;; What does cons take as its arguments?
;; -> 1st arg, any s-expression. 2nd arg, any list.

(check-expect
 (let ([s '(a b (c))]
       [l '()])
   (cons s l))
 '((a b (c))))

(check-expect
 (let ([s 'a]
       [l '()])
   (cons s l))
 '(a))

(check-error
 (let ([s '((a b c))]
       [l 'b])
   (cons s l))
 "cons: second argument must be a list, but received (list (list 'a 'b 'c)) and 'b")

(check-error
 (let ([s 'a]
       [l 'b])
   (cons s l)) 
 "cons: second argument must be a list, but received 'a and 'b")

; p.9

;; The Law of Cons: The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.

(check-expect
 (let ([s 'a]
       [l '((b) c d)])
   (cons s (car l))) 
 '(a b))

(check-expect
 (let ([s 'a]
       [l '((b) c d)])
   (cons s (cdr l)))
 '(a c d))

;; Is it true that the list l is the null list where l is ()?
;; True

(check-expect (null? (quote ())) #true)

(check-expect  (null? '(a b c)) #false)

; p.10

(check-expect
 (let ([a 'spaghetti])
   (null? a))
 #false)

;; The Law of Null?: The primitive null? is defined only for lists

(check-expect
 (let ([s 'Harry])
   (atom? s))
 #true)

(check-expect
 (let ([s '(Harry had a heap of apples)])
   (atom? s))
 #false)

;; How many arguments does atom? take and what are they?
;; -> One arg, any s-expression.

; p.11
(check-expect
 (let ([l '(Harry had a heap of apples)])
   (atom? (car l)))
 #true)

(check-expect
 (let ([l '(Harry had a heap of apples)])
   (atom? (cdr l)))
 #false)

(check-expect
 (let ([l '(Harry)])
   (atom? (cdr l)))
 #false)

(check-expect
 (let ([l '(swing low sweet cherry oat)])
   (atom? (car (cdr l))))
 #true)

(check-expect
 (let ([l '(swing (low sweet) cherry oat)])
   (atom? (car (cdr l))))
 #false)

(check-expect
 (let ([a1 'Harry]
       [a2 'Harry])
   (eq? a1 a2))
 #true)

(check-expect
 (let ([a1 'margarine]
       [a2 'butter])
   (eq? a1 a2))
 #false)

; p.12

;; How many arguments does eq? take and what are they?
;; -> It takes two arguments.  Both of them must be non-numeric atoms.

(check-expect
 (let ([l1 '()]
       [l2 '(strawberry)])
   (eq? l1 l2))
 #false)

(check-expect
 (let ([n1 6]
       [n2 7])
   (eq? n1 n2)) ; In practice, some numbers may be arguments of eq?.
 #false)

;; The Law of Eq?: The primitive eq? takes two arguments. Each must be a non-numeric atom.

(check-expect
 (let ([l '(Mary had a little lamb chop)]
       [a 'Mary])
   (eq? (car l) a))
 #true)

(check-expect
 (let ([l '(soured milk)]
       [a 'milk])
   (eq? (cdr l) a))
 #false)

; p.13

(check-expect
 (let ([l '(beans beans we need jelly beans)])
   (eq? (car l) (car (cdr l))))
 #true)

(test)

