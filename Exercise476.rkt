;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise476) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])

; An FSM is a structure:
;  (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;  (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.

; data example: see exercise 109

(define fsm-abcd
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean
; does an-fsm recognize the given string
(check-expect (fsm-match? fsm-abcd "acbd")
              #true)
(define (fsm-match? an-fsm a-string)
  (equal? (fsm-final an-fsm)
          (transition-to-end (fsm-initial an-fsm)
                             (explode a-string)
                             (fsm-transitions an-fsm))))

(check-expect (transition-to-end "AA" '("a" "b" "c" "d") (fsm-transitions fsm-abcd))
              "DD")
(check-expect (transition-to-end "AA" '("a" "c" "b" "d") (fsm-transitions fsm-abcd))
              "DD")
(define (transition-to-end current actions transitions)
  (cond
    [(empty? actions) current]
    [else
     (local ((define nextState (findNextState current
                                             (first actions)
                                             transitions)))
       (if (boolean? nextState) ; 是boolean 说明没找到下个状态
           #false
           (transition-to-end nextState (rest actions) transitions)))]))

(check-expect (findNextState "AA" "a" (fsm-transitions fsm-abcd))
              "BC")
(check-expect (findNextState "BC" "b" (fsm-transitions fsm-abcd))
              "BC")
(check-expect (findNextState "BC" "c" (fsm-transitions fsm-abcd))
              "BC")
(check-expect (findNextState "BC" "d" (fsm-transitions fsm-abcd))
              "DD")
(define (isMyTransition state action transition)
  (and (equal? state (transition-current transition))
       (equal? action (transition-key transition))))
(define (findNextState state action fsm-transitions)
  (cond
    [(empty? fsm-transitions) #false]
    [else
     (if (isMyTransition state action (first fsm-transitions))
         (transition-next (first fsm-transitions))
         (findNextState state action (rest fsm-transitions)))]))