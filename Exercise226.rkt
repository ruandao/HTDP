;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Exercise226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)

; An FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;  (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation An  FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))
(define (state=? x) #true)

(define fsm-bw
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; FSM -> ???
; match the keys pressed with the given FSM
(define (simulate an-fsm)
  (big-bang ...
    [to-draw ...]
    [on-key ...]))

(define (simulate.v1 fsm0)
  (big-bang initial-state
    [to-draw render-state.v1]
    [on-key find-next-state.v1]))

; SimulationState.v1 -> Image
; renders a world state as an image
(define (render-state.v1 s)
  empty-image)

; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
  cs)

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure:
;  (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image
(define (render-state.v2 s)
  empty-image)

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(define (find-next-state.v2 cs ke)
  cs)

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

(simulate.v2 fsm-traffic "red")

; SimulationState.v2 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current as-fsm)))

(check-expect
 (find-next-state (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "red") "a")
 (make-fs fsm-traffic "green"))
(check-expect
 (find-next-state (make-fs fsm-traffic "green") "q")
 (make-fs fsm-traffic "yellow"))
(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "black")
              "not found: black")
(define (find transitions current)
  (cond
    [(empty? transitions) "not found"]
    [else (if (equal? current
                      (transition-current (first transitions)))
              (transition-next (first transitions))
              (find (rest transitions) current))]))

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;  (make-ktransition FSM-State KeyEvent FSM-State)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure:
;  (make-fsm FSM-State LOT FSM-State)
; A LOT is one of:
; - '()
; - (cons Transition.v3 LOT)
; A Transitoin.v3 is a structure:
;  (make-transition FSM-State KeyEvent FSM-State)

  current)
