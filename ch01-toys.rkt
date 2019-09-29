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
(check-expect (atom? '(atom))
              #false)
(check-expect (atom? '(atom turkey or))
              #false)

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
(check-expect (car '(a b c))
              'a)
(check-expect (car '((a b c) x y z))
              '(a b c))
(check-error (car 'hotdog)
             "car: expects a pair, given 'hotdog")
(check-error (car '())
             "car: expects a pair, given '()")

; p.6
(check-expect (car '(((hotdogs)) (and) (pickle) relish))
              '((hotdogs)))
(check-expect (car (car '(((hotdogs)) (and))))
              '(hotdogs))
(check-expect (cdr '(a b c))
              '(b c))
(check-expect (cdr '((a b c) x y z))
              '(x y z))
(check-expect (cdr '(hamburger))
              '())
(check-expect (cdr '((x) t r))
              '(t r))
(check-error (cdr 'hotdogs)
             "cdr: expects a pair, given 'hotdogs")

; p.7
(check-error (cdr '())
             "cdr: expects a pair, given '()")
(check-expect (car (cdr '((b) (x y) ((c)))))
              '(x y))
(check-expect (cdr (cdr '((b) (x y) ((c)))))
              '(((c))))
(check-error (cdr (car '(a (b (c)) d)))
              "cdr: expects a pair, given 'a")
;; What does car take as an argument?
;; -> a cons cell (a pair)
;; What does cdr take as an argument?
;; -> a cons cell (a pair)
(check-expect (cons 'peanut '(butter and jelly))
              '(peanut butter and jelly))

; p.8
(check-expect (cons '(banana and) '(peanut butter and jelly))
              '((banana and) peanut butter and jelly))

(check-expect (cons '((help) this)
                    '(is very ((hard) to learn)))
              '(((help) this) is very ((hard) to learn)))
;; What does cons take as its arguments?
;; -> 1st arg, any s-expression. 2nd arg, any list)
(check-expect (cons '(a b (c)) '())
              '((a b (c))))
(check-expect (cons 'a '())
              '(a))
(check-error (cons '((a b c)) 'b)
              "cons: second argument must be a list, but received (list (list 'a 'b 'c)) and 'b")
(check-error (cons 'a 'b)
             "cons: second argument must be a list, but received 'a and 'b")

;; The Law of Cons: The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.

; p.9
(check-expect (cons 'a (car '((b) c d)))
              '(a b))
(check-expect (cons 'a (cdr '((b) c d)))
              '(a c d))
;; Is it true that the list l is the null list where l is ()?
;; True
(check-expect (null? (quote ())) #true)
(check-expect  (null? '(a b c)) #false)

; p.10

;; The Law of Null?: The primitive null? is defined only for lists
(check-expect (null? 'spaghetti)
              #false)
(check-expect (atom? 'Harry)
              #true)
(check-expect (atom? 'Harry)
              #true)
(check-expect (atom? '(Harry had a heap of apples)) #false)
;; How many arguments does atom? take and what are they?
;; -> One arg, any s-expression.

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

;; The Law of Eq?: The primitive eq? takes two arguments. Each must be a non-numeric atom.

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

