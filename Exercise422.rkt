;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise422) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))
(check-expect (bundle '("a" "b") 3)
              (list "ab"))
(check-expect (bundle '() 3)
              '())
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (map implode (list->chunks s n)))
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(or (empty? l) (equal? n 0)) '()]
    [else (cons (first l)
                (take (rest l) (- n 1)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(empty? l) '()]
    [(equal? n 0) l]
    [else
     (drop (rest l) (- n 1))]))


(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [(equal? n 0) (error "not support zero n")]
    [else (cons (take l n)
                (list->chunks (drop l n) n))]))