;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise146) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [else (and (> (first l) (first (rest l)))
               (sorted>? (rest l)))]))

(define (how-many nel)
  (cond
    [(empty? (rest nel)) 1]
    [else (+ 1 (how-many (rest nel)))]))

(define (sum nel)
  (cond
    [(empty? (rest nel)) (first nel)]
    [else (+ (first nel) (sum (rest nel)))]))
(define (average l)
  (cond
    [(empty? l) 0]
    [else (/ (sum l) (how-many l))]))