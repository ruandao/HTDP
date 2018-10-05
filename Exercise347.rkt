;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exercise347) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct add [left right])
(define-struct mul [left right])

(define (eval-expression exp)
  (cond
    [(number? exp) exp]
    [(equal? '+ (first exp))
     (+ (eval-expression (second exp))
        (eval-expression (third exp)))]
    [(equal? '* (first exp))
     (* (eval-expression (second exp))
        (eval-expression (third exp)))]))

(eval-expression '(+ 1 1))
(eval-expression '(+ (* 3 3) (* 4 4)))