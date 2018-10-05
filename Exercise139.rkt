;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise139) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)


; A List-of-amounts is one of:
; - '()
; - (cons PositiveNumber List-of-amounts)
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else (+ (first loa) (sum (rest loa)))]))

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else (and (positive? (first lon))
               (pos? (rest lon)))]))
(define (positive? n)
  (>= n 0))

(define (checked-sum lon)
  (cond
    [(pos? lon) (sum lon)]
    [else (error "input should be positive number sequence")]))