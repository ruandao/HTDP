;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise427) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define threshold 7)

(check-expect (sort< '(9 8 4 28 3 282 38 478))
              '( 3 4 8 9 28 38 282 478))
(define (sort< l)
  (cond
    [(empty? l) '()]
    [else (insert< (first l) (sort< (rest l)))]))

(check-expect (insert< 4 '(1 2 3 4 5 6))
              '(1 2 3 4 4 5 6))
(define (insert< n l)
  (cond
    [(empty? l) (list n)]
    [else (if (< n (first l))
              (cons n l)
              (cons (first l) (insert< n (rest l))))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< '(12 12 12 28 39 82 58 27 59))
              '(12 12 12 27 28 39 58 59 82))
(define (quick-sort< alon)
  (cond
    [(< (length alon) threshold) (sort< alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers (rest alon) pivot))
                    (cons pivot
                          (quick-sort< (largers= (rest alon) pivot)))))]))


; [List-of Number] Number -> [List-of Number]
(check-expect (largers= '(12 13 14 11 15 12 10) 12)
              '(12 13 14 15 12))
(check-expect (largers= '(11 10 12 12 13 11 15 10) 12)
              '(12 12 13 15))
(define (largers= alon n)
  (cond
    [(empty? alon) '()]
    [else (if (>= (first alon) n)
              (cons (first alon) (largers= (rest alon) n))
              (largers= (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
(check-expect (smallers '(12 13 12 14 15) 12)
              '())
(check-expect (smallers '(12 1 11 10 8 14 15) 12)
              '(1 11 10 8))
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

