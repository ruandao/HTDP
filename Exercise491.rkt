;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise491) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin

(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))

; Number [List-of Number] -> [List-of Number]
; adds n to each number on l

(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))

(define (add-to-each n l)
  (map (lambda(x)
         (+ n x))
       l))

(define (relative->absolute/a l accu-dist)
  (cond
    [(empty? l) '()]
    [else
     (local ((define tally (+ (first l) accu-dist)))
       (cons tally
             (relative->absolute/a (rest l) tally)))]))


; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin

(check-expect (relative-absolute.v2 '(50 40 70 30 30))
              '(50 90 160 190 220))

(define (relative-absolute.v2 l0)
  (local (
          ; [List-of Number] Number -> [List-of Number]
          (define (relative->absolute/a l accu-dist)
            (cond
              [(empty? l) '()]
              [else
               (local ((define accu (+ (first l) accu-dist)))
                 (cons accu
                       (relative->absolute/a (rest l) accu)))])))
    (relative->absolute/a l0 0)))

(define (relative->absolute.v3 l)
  (reverse.v1
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse.v1 (rest l)))))

(define (reverse.v1 l)
  (cond
    [(empty? l) '()]
    [else (add-to-end (first l) (rest l))]))
(define (add-to-end x l)
  (cond
    [(empty? l) (list x)]
    [else (cons (first l) (add-to-end x (rest l)))]))


; A SimpleGraph is a [List-of Connection]
; A Connection is a list of two items:
;  (list Node Node)
; A Node is a Symbol.
(define a-sg
  '((a b)
    (b c)
    (c e)
    (d e)
    (e b)
    (f f)))

; Node Node SimpleGraph -> Boolean
; is there a path from origin to destination
; in the simple graph sg
(check-expect (path-exists? 'a 'e a-sg) #true)
(check-expect (path-exists? 'a 'f a-sg) #false)

(define (find-map-item key m)
  (cond
    [(empty? m) #false]
    [else
     (if (equal? key (first (first m)))
         (first m)
         (find-map-item key (rest m)))]))

(define (path-exists? origin destination sg)
  (local ((define (path-exists/a? src dst sg crossed)
            (local ((define hadVisitedItem (find-map-item src crossed))
                    (define item (find-map-item src sg)))
              (cond
                [(cons? hadVisitedItem) #false]
                [(equal? (second item) dst) #true]
                [else (path-exists/a? (second item) dst sg (cons item crossed))]))))
    (path-exists/a? origin destination sg '())))


; [List-of X] -> [List-of X]
; constructs the reverse of alox
;(check-expect (invert '(a b c)) '(c b a))
(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else
     (add-as-last (first alox) (invert (rest alox)))]))

; X [List-of X] -> [List-of X]
; adds an-x to the end of alox
;(check-expect (add-as-last 'a '(c b)) '(c b a))
(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons (first alox) (add-as-last (rest alox)))]))

(define (invert.v2 alox0)
  (local (; [List-of X] ??? -> [List-of X]
          ; constructs the reverse of alox
          ; accumulator ...
          (define (invert/a alox a)
            (cond
              [(empty? alox) a]
              [else (invert/a (rest alox) (cons (first alox) a))])))
    (invert/a alox0 '())))


(define (!.v1 n)
  (local ((define (!.v1/a n base)
            (cond
              [(equal? n 1) base]
              [else (!.v1/a (- n 1) (* n base))])))
    (!.v1/a n 1)))

(define (!.v0 n)
  (cond
    [(equal? n 1) 1]
    [else (* n (!.v0 (- n 1)))]))

(define (run f n)
  (cond
    [(equal? n 0) '()]
    [else (f (run f (- n 1)))]))


(define-struct node [left right])
; A Tree is one of:
; - '()
; - (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+1 (max (height (node-left abt))
                   (height (node-right abt))))]))

; N -> Number
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
 (mirror (explode "abc")) (explode "abcba"))
(define (mirror nel)
  (local ((define (mirror/a nel post)
            (cond
              [(empty? (rest nel)) (cons (first nel) post)]
              [else (cons (first nel)
                          (mirror/a (rest nel) (cons (first nel) post)))])))
    (mirror/a nel '())))
(define (last nel)
  (cond
    [(empty? (rest nel)) (first nel)]
    [else (last (rest nel))]))
(define (mirror/b s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

(define (all-but-last nel)
  (cond
    [(empty? (rest nel)) (list (first nel))]
    [else (cons (first nel) (all-but-last (rest nel)))]))

; Matrix -> Matrix
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place
; no termination if all rows start with 0
;(check-expect (rotate-until.v2 '((0 4 5) (1 2 3)))
;              '((1 2 3) (0 4 5)))
(define (rotate1 M)
  (cond
    [(not (= (first (first M)) 0)) M]
    [else
     (rotate (append (rest M) (list (first M))))]))

(define (rotate.v2 M0)
  (local (; Matrix ... -> Matrix
          ; accumulator ...
          (define (rotate/a M seen)
            (cond
              [(empty? M) seen]
              [(not (= 0 (first (first M)))) (append M seen)]
              [else (rotate/a (rest M)
                              (cons (first M) seen))])))
    (rotate/a M0 '())))

(define (power n m)
  (cond
    [(zero? m) 1]
    [else (* n (power n (sub1 m)))]))
(define (to10 l)
  (local ((define (to10/a reverseL idx result)
            (cond
              [(empty? reverseL) result]
              [else (to10/a (rest reverseL) (add1 idx) (+ result (* (first reverseL)
                                                                    (power 10 idx))))])))
    (to10/a (reverse l) 0 0)))

(define (to10.v2 l)
  (local ((define (to10.v2/a l pre)
            (cond
              [(empty? l) pre]
              [else (to10.v2/a (rest l) (+ (first l)
                                           (* 10 pre)))])))
    (to10.v2/a l 0)))

(define (is-prime n)
  (local ((define (is-prime/a from to n)
            (cond
              [(> from to) #true]
              [else
               (if (= 0 (remainder n from))
                   #false
                   (is-prime (+ 1 from) to n))])))
    (cond
      [(= 1 n) #true]
      [(= 2 n) #true]
      [else 
       (is-prime/a 2 (/ n 2) n)])))
