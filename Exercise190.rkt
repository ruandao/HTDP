;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)


(define (prefixes p l)
  (cond
    [(empty? p) #true]
    [(empty? l) #false]
    [else (or (equal? (first p)
                      (first l))
              (prefixes (rest p)
                        (rest l)))]))


(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else (cons l
                (suffixes (rest l)))]))