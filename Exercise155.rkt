;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise155) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define-struct layer [color doll])
; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; RD -> Number
; how many dolls are part of an-rd
(check-expect (depth "red") 1)
(check-expect
  (depth
   (make-layer "yellow" (make-layer "green" "red")))
  3)
(define (depth dolls)
  (cond
    [(string? dolls) 1]
    [(layer? dolls) (+ 1 (depth (layer-doll dolls)))]))

(check-expect (colors (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")
(define (get-color x)
  x)
(define (colors an-Rd)
  (cond
    [(string? an-Rd) (get-color an-Rd)]
    [(layer? an-Rd) (string-append (get-color (layer-color an-Rd))
                                   ", "
                                   (colors (layer-doll an-Rd)))]))

(check-expect (inner (make-layer "yellow" (make-layer "green" "red")))
              "red")
(define (inner rd)
  (cond
    [(string? rd) rd]
    [(layer? rd) (inner (layer-doll rd))]))