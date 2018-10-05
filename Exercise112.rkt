;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise112) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; Number -> Number
; computes the area of a disk with radius r
(define (area-of-disk r)
  (* 3.14 (* r r)))
; Any -> Number
; computes the area of a disk with radius v,
; if v is a number
(define (checked-area-of-disk v)
  (cond
    [(number? v) (area-of-disk v)]
    [else (error "area-of-disk: number expected")]))

(define (PositiveNumber? n)
  (< 0 n))
(define (checked-area-of-disk.v2 v)
  (cond
    [(number? v)
     (cond
       [(PositiveNumber? v) (area-of-disk v)]
       [else (error "area-of-disk: positive number expected")])]
    [else (error "area-of-disk: number expected")]))


(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector
(defince (checked-make-vec x y)
  (cond
    [(and (number? x) (PositiveNumber? x)
          (number? y) (PositiveNumber? y))
     (make-vec x y)]
    [else (error "checked-mke-vec: PositiveNumber, PositiveNumber expected")]))


; Any -> Boolean
; is a an element of the MissileOrNot collection
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(define (missile-or-not? a)
  (cond
    [(posn? a) #true]
    [(boolean? a) (not a)]
    [else #false]))
