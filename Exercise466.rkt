;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise466) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
  (local ((define multiple (/ (first (lhs e1)) (first (lhs e2))))
          (define multiple-matrix (map (lambda (x) multiple) e2))
          (define e2-multiple (matrix-multiple e2 multiple-matrix))
          (define afterSub (matrix-sub e1 e2-multiple))
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


; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;  n + 1, n, n - 1, ..., 2.
; intepretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations
; 需要考虑到SOE矩阵的第一行的第一个元素,可能是0, 甚至所有行的第一个元素都是0
; 要处理的化,要求先把matrix 整理下,让前一行的元素个数大于等于后一行的元素个数
; 如果行的第一个元素是0, 那么缩短行元素的长度
(check-expect (triangulate '((2 2 3 10) (2 5 12 31)))
              '((2 2 3 10) (3 9 21)))
(define (triangulate M)
  (local ((define strictM (strict-soe M))
          (define fL (first strictM))
          (define restL (map (lambda (l)
                               (if (not (equal? (length l) (length fL)))
                                   l
                                   (subtract l fL)))
                             (rest strictM))))
    (cond
      [(equal? (length restL) 1) (cons fL restL)]
      [else (cons fL (triangulate restL))])))

; make sure the previous element's length was large or equal than the rest
; make sure each elem's first item was not zero
(define (strict-soe m)
  (local ((define (eliminate-zero l)
            (cond
              [(empty? l) '()]
              [else (if (equal? (first l) 0)
                        (eliminate-zero (rest l))
                        l)]))
          (define e-zero-m (map eliminate-zero m))
          (define eliminate-empty-line (filter (lambda (l)
                                                (not (equal? 0 (length l))))
                                              e-zero-m)))
    (sort eliminate-empty-line
          (lambda (l1 l2)
            (> (length l1) (length l2))))))
(define M2
  '((2 2 2 6)
    (2 2 4 8)
    (2 2 1 2)))

(triangulate M2)