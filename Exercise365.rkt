;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise365) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; - (cons Symbol [List-of Xexpr.v2])
; - (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '())

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) a0)
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) a0)
(define (xexpr-attr xe)
  (cond
    [(is-body? (rest xe)) '()] ; (cons Symbol Body)
    [else (second xe)] ; (cons Symbol (cons [List-of Attribute] Body)
    ))
(define (is-body? attr-or-body)
  (cond
    [(empty? attr-or-body) #true] ; '()
    [(empty? (first attr-or-body)) #false]
    [(symbol? (first (first attr-or-body))) #true] ; (cons Xexpr.v2 Body)
    [else #false] ; (not symbol? (first (first attr-or-body))) is attr
    ))



