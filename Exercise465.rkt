;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise465) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers.
; interpretation if (list a1 ... an b) is an Equation,
; a1, ..., an are the left-hand-side variable coefficients
; and b is the right-hand side

; A Solution is a [List-of Number]

(define M  ; an SOE
  (list (list 2 2 3 10) ; An Equation
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define S '(1 1 2)) ; A Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

(define (check-solution soe s)
  (andmap (lambda (equation)
            (equal? (plug-in (lhs equation) s)
                    (rhs equation))
            ) soe))

(define (plug-in l1 l2)
  (cond
    [(empty? l1) 0]
    [else (+ (* (first l1) (first l2))
             (plug-in (rest l1) (rest l2)))]))

(define (subtract e1 e2)
  (local ((define multiple (/ (first (lhs e1)) (first lhs e2)))
          (define afterSub (matrix-sub e1 (matrix-multiple e2 (map (lambda (x) multiple) e2))))
          )
    (rest afterSub)))

(define (matrix-sub m1 m2)
  (cond
    [(empty? m1) '()]
    [else (cons (- (first m1) (first m2))
                (matrix-sub (rest m1) (rest m2)))]))
(define (matrix-multiple col row)
  (cond
    [(empty? col) '()]
    [else (cons (* (first col) (first row))
                (matrix-multiple (rest col) (rest row)))]))

