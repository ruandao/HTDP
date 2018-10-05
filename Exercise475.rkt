;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise475) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; A Node is a Symbol.

; A Graph is:
; - '()
; - (cons (list Node [List-of Node]) Graph)

; A route is:
; (list Node [List-of Node])
(define (route-src r)
  (first r))
(define (route-dsts r)
  (second r))

(define (neighbors n g)
  (cond
    [(empty? g) '()]
    [(equal? n (route-src (first g))) (route-dsts (first g))]
    [else (neighbors n (rest g))]))

; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
(define (find-path src dst g)
  (cond
    [(empty? g) '()]
    [(equal? src dst) (list src)]
    [(equal? src (route-src (first g)))
     (local ((define findP (find-path-m (route-dsts (first g)) dst g)))
       (if (empty? findP)
           '()
           (cons src findP)))]
    [else (find-path src dst (rest g))]))
(define (find-path-m srcs dst g)
  (cond
    [(empty? g) '()]
    [(empty? srcs) '()]
    [else
     (local ((define fp (find-path (first srcs) dst g)))
       (if (empty? fp)
           (find-path-m (rest srcs) dst g)
           fp))]))

(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(find-path 'C 'D sample-graph)
(find-path 'E 'D sample-graph)
(find-path 'C 'G sample-graph)

; A Path is a [List-of Node]
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first
; Node on the list to the last one.

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
 
(define (find-path origination destination G)
  #false)

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-originations to
; destination; otherwise, it produces #false
(define (find-path/list lo-originations destination G)
  #false)

