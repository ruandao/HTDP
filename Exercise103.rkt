;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise103) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Exercise 103

(define-struct spider [nLegs space])
(define-struct elephant [space])
(define-struct boa [length girth])
(define-struct armadillo [space])

(define (boa-space boa)
  (* (boa-length boa)
     (boa-girth boa)))


(define (fit-spider? spider cageDesc)
  (> (spider-space spider) cageDesc))
(define (fit-elephant? elephant cageDesc)
  (> (elephant-space elephant) cageDesc))
(define (fit-boa? boa cageDesc)
  (> (boa-space boa) cageDesc))
(define (fit-armadillo? armadillo cageDesc)
  (> (armadillo-space armadillo) cageDesc))

(define (fits? animal cageDesc)
  (cond
    [(spider? animal)
     (fit-spider? animal cageDesc)]
    [(elephant? animal)
     (fit-elephant? animal cageDesc)]
    [(boa? animal)
     (fit-boa? animal cageDesc)]
    [(armadillo? animal)
     (fit-armadillo? animal cageDesc)]))