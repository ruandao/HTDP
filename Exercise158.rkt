;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise158) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define HEIGHT 80); distances in terms of pixels
(define WIDTH 100);
(define MID (/ WIDTH 2))
(define XSHOTS (/ WIDTH 2))

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A List-of-shots is one of:
; - '()
; - (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number.
; interpretation represents the shot's y-coordinate

; A ShotWorld is List-of-numbers.
; interpretation each number on such a list
;  represents the y-coordinate of a shot

; ShotWorld -> Image
; adds the image of a shot for each y on w
; at (MID, y) to the background image
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT
                       MID (first w)
                       (to-image (rest w)))]))

(define (is-out-shot s)
  (<= s 0))
; eliminate-out-shot
; if a shot is above 0; cancel it
(check-expect (eliminate-out-shot (cons 9 (cons 0 '())))
              (cons 9 '()))
(define (eliminate-out-shot w)
  (cond
    [(empty? w) w]
    [else
     (if (is-out-shot (first w))
         (eliminate-out-shot (rest w))
         (cons (first w) (eliminate-out-shot (rest w))))]))
; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (up-shot 3) 2)
(define (up-shot s)
  (- s 1))
(check-expect (tock '()) '())
(check-expect (tock (cons 4 '())) (cons 3 '()))
(define (tock w)
  (cond
    [(empty? w) w]
    [else (eliminate-out-shot (cons (up-shot (first w))
                (tock (rest w))))]))

; ShotWorld KeyEvent -> ShotWorld
; adds a shot to the world
; if the player presses the space bar
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh '() "k") '())
(define (keyh w ke)
  (cond
    [(equal? ke " ")
     (cons HEIGHT w)]
    [else w]))

(define (main w)
  (big-bang w
    [on-draw to-image]
    [on-tick tock]
    [on-key keyh]))

(main '())