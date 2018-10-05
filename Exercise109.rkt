;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; state:
; - AA "start, expect an 'a'"
; - BB "expect 'b', 'c', or 'd'"
; - DD "finished"
; - ER "error, illegal key"
(define-struct abcd [state str])

(define AA "a")
(define BB "b")
(define DD "d")
(define ER "er")
(define DefaultState (make-abcd AA ""))

(define BG-width 100)
(define BG-height 100)
(define Font-size 16)

(define (AA? x)
  (equal? (abcd-state x) AA))
(define (BB? x)
  (equal? (abcd-state x) BB))
(define (DD? x)
  (equal? (abcd-state x) DD))
(define (ER? x)
  (equal? (abcd-state x) ER))

(define (AA-next s ke)
  (cond
    [(equal? ke "a") (make-abcd BB "a")]
    [else (make-abcd ER ke)]))
(define (BB-next s ke)
  (cond
    [(or (equal? ke "b")
         (equal? ke "c"))
     (make-abcd BB (string-append (abcd-str s) ke))]
    [(equal? ke "d")
     (make-abcd DD (string-append (abcd-str s) ke))]
    [else (make-abcd ER (string-append (abcd-str s) ke))]))
(define (abcd-key s ke)
  (cond
    [(AA? s) (AA-next s ke)]
    [(BB? s) (BB-next s ke)]
    [(DD? s) s]
    [(ER? s) s]))


(define (rectangle-rgb color)
  (rectangle BG-width BG-height "outline" color))
(define (abcd-render-bg s)
  (cond
    [(AA? s) (rectangle-rgb "white")]
    [(BB? s) (rectangle-rgb "yellow")]
    [(DD? s) (rectangle-rgb "green")]
    [(ER? s) (rectangle-rgb "red")]))
(define (abcd-render s)
  (place-image (text (abcd-str s) Font-size "black")
               (/ BG-width 2) (/ BG-height 2)
               (abcd-render-bg s)))

(define (main s)
  (big-bang s
    [on-draw abcd-render]
    [on-key abcd-key]))

(main DefaultState)