;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise430) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define threshold 7)

(check-expect (sort '(9 8 4 28 3 282 38 478) <)
              '( 3 4 8 9 28 38 282 478))
;(define (sort less l)
;  (cond
;    [(empty? l) '()]
;    [else (insert less (first l) (sort less (rest l)))]))

(check-expect (insert < 4 '(1 2 3 4 5 6))
              '(1 2 3 4 4 5 6))
(define (insert less n l)
  (cond
    [(empty? l) (list n)]
    [else (if (less n (first l))
              (cons n l)
              (cons (first l) (insert less n (rest l))))]))

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort < '(12 12 12 28 39 82 58 27 59))
              '(12 12 12 27 28 39 58 59 82))
(define (quick-sort less alon)
  (cond
    [(< (length alon) threshold) (sort alon less)]
    [else (local ((define pivot (first alon))
                  (define (small x) (less x pivot))
                  (define (large x) (not (small x))))
            (append (quick-sort less (filter small (rest alon)))
                    (cons pivot
                          (quick-sort less (filter large (rest alon))))))]))


