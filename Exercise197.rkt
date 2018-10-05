;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise197) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;(count-by-letter AS-LIST)

(define (most-frequent dict)
  (cond
    [(empty? dict) (error "not support empty dict")]
    [else (WL-letter
           (first
            (sort
             ; WL-sort
             (weightsLetters->WLs (count-by-letterX LETTERS dict) LETTERS))))]))

(define-struct WL [weight letter])
(define (sort l)
  (cond
    [(empty? l) '()]
    [else (insert (first l) (sort (rest l)))]))
(define (insert  item l)
  (cond
    [(empty? l) (list item)]
    [else (if (WL-sort item (first l))
              (cons (first l) (insert  item (rest l)))
              (cons item l))]))
(define (WL-sort WL1 WL2)
  (< (WL-weight WL1) (WL-weight WL2)))

(define (weightsLetters->WLs weights letters)
  (cond
    [(empty? weights) '()]
    [else (cons (make-WL (first weights) (first letters))
                (weightsLetters->WLs (rest weights) (rest letters)))]))


(most-frequent AS-LIST)