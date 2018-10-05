;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise479) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define QUEENS 8)
; A QP is a structure:
;  (make-posn CI CI)
; A CI is an N in [0, QUEENS).
; interpretation (make-posn r c) denotes the square at
; the r-th row and c-th column

; will threaten each other?
(define (threatening? qp1 qp2)
  (and (not (horizontal-threaten? qp1 qp2))
       (not (vertical-threaten? qp1 qp2))
       (not (diagonal-threaten? qp1 qp2))))

(define (horizontal-threaten? qp1 qp2)
  (equal? (posn-y qp1) (posn-y qp2)))

(define (vertical-threaten? qp1 qp2)
  (equal? (posn-x qp1) (posn-x qp2)))

(define (diagonal-threaten? qp1 qp2)
  (or (same-sum qp1 qp2)
      (subtract-equal? qp1 qp2)))

(define (same-sum p1 p2)
  (equal? (+ (posn-x p1) (posn-y p1))
          (+ (posn-x p2) (posn-y p2))))
(define (subtract-equal? p1 p2)
  (equal? (- (posn-x p1) (posn-x p2))
          (- (posn-y p1) (posn-y p2))))
