;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise151) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

(check-expect (add 3) 3)
(define (add n)
  (cond
    [(zero? n) 0]
    [else (add1 (sub1 n))]))

(check-expect (multiply 3 4) 12)
(define (multiply n x)
  (cond
    [(zero? n) 0]
    [else (+ (multiply (sub1 n) x) x)]))