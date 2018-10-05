;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise381) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define Font-Size 15)
(define Color "black")
; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;  (cons FSM-State (cons key (cons FSM-State '())))
; An FSM-State is a String that specifies a color

; data examples
(define fsm-traffic
  '(("red" ( "a" "green"))
    ("green" ( "b" "yellow"))
    ("yellow" ( "c" "red"))))

; FSM FSM-State -> FSM-State
(define (simulate state0 transitions)
  (big-bang state0; FSM-State
    [to-draw
     (lambda (current)
       (place-image (text current Font-Size Color)
                    50 50
                    (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (local ((define ret (find transitions current)))
         (cond
           [(and (cons? ret)
                 (equal? (first ret) key-event))
            (second ret)]
           [else current])))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") '("a" "green"))
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

