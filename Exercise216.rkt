;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise216) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)


; 215 很酷...

(define BG-width 100)
(define BG-height 100)
(define BG (empty-scene BG-width BG-height))
(define Speed 1)
(define Worm [circle 3 "solid" "red"])
(define Food [rectangle 2 2 "outline" "green"])

(define-struct world [ worm food ])
(define-struct worm [ pos direction ])

; 改变蠕虫的方向
(define (make-worm->left world)
  (make-world (make-worm (worm-pos (world-worm world))
                         "left")
              (world-food world)))
(define (make-worm->right world)
  (make-world (make-worm (worm-pos (world-worm world))
                         "right")
              (world-food world)))
(define (make-worm->up world)
  (make-world (make-worm (worm-pos (world-worm world))
                         "up")
              (world-food world)))
(define (make-worm->down world)
  (make-world (make-worm (worm-pos (world-worm world))
                         "down")
              (world-food world)))


; 移动蠕虫
(define (move-worm-X worm Speed)
  (cond
    [(equal? (worm-direction worm) "left")
     (make-worm (make-posn (- (posn-x (worm-pos worm)) Speed)
                           (posn-y (worm-pos worm)))
                (worm-direction worm))]
    [(equal? (worm-direction worm) "right")
     (make-worm (make-posn (+ (posn-x (worm-pos worm)) Speed)
                           (posn-y (worm-pos worm)))
                (worm-direction worm))]
    [(equal? (worm-direction worm) "up")
     (make-worm (make-posn (posn-x (worm-pos worm))
                           (- (posn-y (worm-pos worm)) Speed))
                (worm-direction worm))]
    [(equal? (worm-direction worm) "down")
     (make-worm (make-posn (posn-x (worm-pos worm))
                           (+ (posn-y (worm-pos worm)) Speed))
                (worm-direction worm))]))
; 要考虑是否和食物接触到,接触到,需要产生下一个食物, 并且增长
; ... 先放弃上面的想法,跟着教程,一步步实现,看下教程是怎么指导的
; 先制作一个会移动的点就好
(define (move-worm s)
  (make-world (move-worm-X (world-worm s) Speed)
              (world-food s)))

(define (last-world s)
  (place-image (text "worm hit border" 12 "red")
               (/ BG-width 2) (/ BG-height 2)
               BG))
(define (world-stop? s)
  (not (and
        (<= 0 (posn-x (worm-pos (world-worm s))) BG-width)
        (<= 0 (posn-y (worm-pos (world-worm s))) BG-height))))

(define (world-draw s)
  (place-image Worm
               (posn-x (worm-pos (world-worm s)))
               (posn-y (worm-pos (world-worm s)))
               (place-image Food
                            (posn-x (world-food s))
                            (posn-y (world-food s))
                            BG)))
(define (world-tick s)
  (move-worm s))
; 键盘改变蠕虫移动的方向
(define (world-key s ke)
  (cond
    [(equal? ke "left") (make-worm->left s)]
    [(equal? ke "right") (make-worm->right s)]
    [(equal? ke "up") (make-worm->up s)]
    [(equal? ke "down") (make-worm->down s)]
    [else s]))

(define (main s)
  (big-bang s
    [on-draw world-draw]
    [on-tick world-tick]
    [stop-when world-stop? last-world]
    [on-key world-key]))

(main (make-world (make-worm (make-posn 4 15) "down")
                  (make-posn 40 60)))