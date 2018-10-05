;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise520) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides
; termination ???
(define TestDeep 4)

(define (solve* los)
  (cond
    [(empty? los) (error "can't solve")]
    [(ormap final? los)
     (first (filter final? los))]
    [else
     (solve* (create-next-states los))]))
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          )
    (solve* (list state0))))

(define-struct Along [mission cannibal])
(define-struct State [left right boat pre])
; pre is:
; - #false
; - state
(define initial-puzzle (make-State (make-Along 3 3) (make-Along 0 0) "left" #false))
(define final-puzzle (make-State (make-Along 0 0) (make-Along 3 3) "right" #false))



(define (not-people along)
  (and (zero? (Along-mission along))
       (zero? (Along-cannibal along))))

(define (exist-along? along)
  (or (zero? (Along-mission along))
      (zero? (Along-mission along))
      (>= (Along-mission along) (Along-cannibal along))))
(define (not-equal-state s1 s2)
  (not (and (equal? (State-left s1) (State-left s2))
            (equal? (State-right s1) (State-right s2))
            (equal? (State-boat s1) (State-boat s2)))))
(define (not-found-in-pre state pre)
  (cond
    [(equal? #false pre) #true]
    [else (and (not-equal-state state pre)
               (not-found-in-pre state (State-pre pre)))]))
(check-expect (s-length initial-puzzle) 1)
(check-expect (s-length (make-State 0 0 0 initial-puzzle)) 2)
(define (s-length s)
  (cond
    [(equal? s #false) 0]
    [else (+ 1 (s-length (State-pre s)))]))

(define (exist? state)
  (and (exist-along? (State-left state))
       (exist-along? (State-right state))
       (not-found-in-pre state (State-pre state))))

(check-expect (final? (make-State (make-Along 0 0) (make-Along 3 3) "right" #false))
              #true)
(define (final? state)
  (not-people (State-left state)))

(define (render-mc state)
  (beside (render-along (State-left state))
          (render-river (State-boat state))
          (render-along (State-right state))))
(define (render-along along) along)
(define (render-river boat) boat)

(check-expect (along-minus-possible (make-Along 1 1) (make-Along 1 1))
              (make-Along 0 0))
(check-expect (along-minus-possible (make-Along 0 1) (make-Along 1 4))
              (make-Along 1 3))
(define (along-minus-possible possible along)
  (make-Along (- (Along-mission along) (Along-mission possible))
              (- (Along-cannibal along) (Along-cannibal possible))))

(check-expect (along-plus-possible (make-Along 1 1) (make-Along 1 1))
              (make-Along 2 2))
(define (along-plus-possible possible along)
  (make-Along (+ (Along-mission along) (Along-mission possible))
              (+ (Along-cannibal along) (Along-cannibal possible))))
; 1 2
; 1 2
(define (all-possible-along along)
  (filter (lambda(possible)
            (and (<= (Along-mission possible) (Along-mission along))
                 (<= (Along-cannibal possible) (Along-cannibal along))))
          (list (make-Along 1 1)
                (make-Along 0 1)
                (make-Along 1 0)
                (make-Along 2 0)
                (make-Along 0 2))))
(define (all-possible state)
  (cond
    [(equal? (State-boat state) "left") (all-possible-along (State-left state))]
    [(equal? (State-boat state) "right") (all-possible-along (State-right state))]))

(define (unwind lol)
  (cond
    [(empty? lol) '()]
    [else (append (first lol)
                  (unwind (rest lol)))]))

(check-expect (s-length (transfer (make-Along 1 1) initial-puzzle))
              (+ 1 (s-length initial-puzzle)))
(define (transfer possible state)
  (cond
    [(equal? (State-boat state) "left")
     (make-State (along-minus-possible possible (State-left state))
                 (along-plus-possible possible (State-right state))
                 "right"
                 state)]
    [(equal? (State-boat state) "right")
     (make-State (along-plus-possible possible (State-left state))                                          
                 (along-minus-possible possible (State-right state))
                 "left"
                 state)]))
(define (create-next-states los)
  (local ((define (create-next-states/a state)
            (filter exist?
                    (map (lambda (possible) (transfer possible state))
                         (all-possible state)))))
    (unwind (map (lambda(state)
                   (create-next-states/a state))
                 los))))



;(check-expect (solve initial-puzzle) final-puzzle)
(solve initial-puzzle)