;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise152) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define (col n img)
  (cond
    [(zero? n) (error "n should be positive")]
    [(= 1 n) img]
    [else (beside (col (sub1 n) img)
                  img)]))
(define (row n img)
  (cond
    [(zero? n) (error "n should be positive")]
    [(= 1 n) img]
    [else (above (row (sub1 n) img)
                 img)]))

(define img (rectangle 10 10 "outline"  "red"))
(row 3 (col 3 img))
