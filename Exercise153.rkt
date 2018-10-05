;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise153) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define (col n img)
  (cond
    [(zero? n) (error "n should be positive")]
    [(= 1 n) img]
    [else (beside (col (sub1 n) img)
                  img)]))
(define (row n img)
  (cond
    [(zero? n) (error "n should be positive")]
    [(= 1 n) img]
    [else (above (row (sub1 n) img)
                 img)]))

(define img (rectangle 10 10 "outline"  "black"))
(define BG (empty-scene (* 8 10) (* 18 10)))
(define BG-width (image-width BG))
(define BG-height (image-height BG))

(define LectureHall (col 8 (row 18 img)))
(define reddot (circle 5 "solid" "red"))

(define BG2 (place-image LectureHall
                         (/ BG-width 2) (/ BG-height 2)
                         BG))

(define (add-balloons lop)
  (cond
    [(empty? lop) BG2]
    [else (place-image reddot
                       (posn-x (first lop)) (posn-y (first lop))
                       (add-balloons (rest lop)))]))

(add-balloons (cons (make-posn 13 14) (cons (make-posn 46 18) '())))