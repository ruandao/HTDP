;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise196) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)


(define LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [else (+ (is-start-with# letter (explode (first dict)))
             (starts-with# letter (rest dict)))]))

(define (is-start-with# letter letterList)
  (cond
    [(empty? letterList) 0]
    [else (if (equal? letter (first letterList))
              1
              0)]))

; (starts-with# "c" AS-LIST) 17406
; (starts-with# "z" AS-LIST) 719
(define (count-by-letter dict)
  (count-by-letterX LETTERS dict))
(define (count-by-letterX l dict)
  (cond
    [(empty? l) '()]
    [else (cons (starts-with# (first l) dict)
                (count-by-letterX (rest l) dict))]))

(count-by-letter AS-LIST)