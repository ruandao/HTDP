;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise159) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define Box-width 4)
(define r (rectangle Box-width Box-width "outline" "black"))
(define (row n img)
  (cond
    [(= n 0) (error "at lease one image for row")]
    [(= n 1) img]
    [else (above img
                 (row (- n 1) img))]))

(define (col n img)
  (cond
    [(= n 0) (error "at lease one image for col")]
    [(= n 1) img]
    [else (beside img
                  (col (- n 1) img))]))

(define BG-col 108)
(define BG-row 200)
;(define BG (col BG-col (row BG-row r)))
(define BG (empty-scene (* Box-width BG-col)
                        (* Box-width BG-row)))
(define Balloon (circle 4 "solid" "red"))

(define (to-img lob)
  (cond
    [(empty? lob) BG]
    [else (place-image Balloon
                       (posn-x (first lob)) (posn-y (first lob))
                       (to-img (rest lob)))]))
(define (is-out balloon)
  (> (posn-y balloon) (* Box-width BG-row)))
(define (dropping balloon)
  (make-posn (posn-x balloon) (+ 1 (posn-y balloon))))
(define (eliminate-out lob)
  (cond
    [(empty? lob) lob]
    [else
     (if (is-out (first lob))
         (eliminate-out (rest lob))
         (cons (first lob) (eliminate-out (rest lob))))]))
(define (tock lob)
  (cond
    [(empty? lob) lob]
    [else (eliminate-out (cons (dropping (first lob))
                               (tock (rest lob))))]))
; pair
(define-struct pair [balloon# lob])
; pair -> n-element-balloon
(define (riot pair)
  (cond
    [(= 0 (pair-balloon# pair)) (pair-lob pair)]
    [else (riot
           (make-pair
            (- (pair-balloon# pair) 1)
            (cons (make-posn (random (* Box-width BG-col)) (random (* Box-width BG-row)))
                  (pair-lob pair))))]))


(big-bang (riot (make-pair 15 '()))
       [on-draw to-img]
       [on-tick tock (/ 1 60)])
    