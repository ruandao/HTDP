;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise105) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct coordinate [x])
(define (yAxis? coordinate)
  (and (not (posn? coordinate)) (> 0 coordinate)))
(define (xAxis? coordinate)
  (and (not (posn? coordinate)) (< 0 coordinate)))
(define (point? coordinate)
  (posn? coordinate))