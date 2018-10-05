;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String



(define TS
  (list
   (list (list "TS/Libs/Code/hang" "TS/Libs/Code/draw")
         (list "TS/Libs/Docs/read!"))
   (list "TS/Text/part1" "TS/Text/part2" "TS/Text/part3")
   "TS/read!"))

(define (how-many? Dir.v1)
  (cond
    [(empty? Dir.v1) 0]
    [(string? (first Dir.v1))
     (+ 1
        (how-many? (rest Dir.v1)))]
    [else (+ (how-many? (first Dir.v1))
             (how-many? (rest Dir.v1)))]))

(how-many? TS)