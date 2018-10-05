;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise217-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)


; 这个贪吃蛇的设计, 每次移动,都在头部加上一个点,然后去除尾部的点

(define BG-width 300)
(define BG-height 300)
(define BG (empty-scene BG-width BG-height))
(define Speed 1)
(define Worm [circle 3 "solid" "red"])
(define Food [rectangle 2 2 "outline" "green"])

(define-struct world [ worm food ])

; 一个贪吃蛇由一系列的点和一个方向组成
(define-struct worm [ lop direction])

(define (add-to-end l p)
  (cond
    [(empty? l) (cons p '())]
    [else (cons (first l)
                (add-to-end (rest l) p))]))


; 改变蠕虫的方向
(define (make-worm->left world)
  (make-world (make-worm (worm-lop (world-worm world))
                         "left")
              (world-food world)))
(define (make-worm->right world)
  (make-world (make-worm (worm-lop (world-worm world))
                         "right")
              (world-food world)))
(define (make-worm->up world)
  (make-world (make-worm (worm-lop (world-worm world))
                         "up")
              (world-food world)))
(define (make-worm->down world)
  (make-world (make-worm (worm-lop (world-worm world))
                         "down")
              (world-food world)))




(define (new-from-old-segment s direction Speed)
  (cond
    [(equal? direction "left")
     (make-posn (- (posn-x s) Speed)
                (posn-y s))]
    [(equal? direction "right")
     (make-posn (+ (posn-x s) Speed)
                (posn-y s))]
    [(equal? direction "up")
     (make-posn (posn-x s)
                (- (posn-y s) Speed))]
    [(equal? direction "down")
     (make-posn (posn-x s)
                (+ (posn-y s) Speed))]))

(define (remove-last l)
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l)
                (remove-last (rest l)))]))
(define (add-to-head l s)
  (cons s l))
; 移动蠕虫
(define (move-worm-next worm Speed)
  (make-worm (add-to-head
              (remove-last (worm-lop worm))
              (new-from-old-segment (first (worm-lop worm))
                                    (worm-direction worm)
                                    Speed))
             (worm-direction worm)))
; 要考虑是否和食物接触到,接触到,需要产生下一个食物, 并且增长
; ... 先放弃上面的想法,跟着教程,一步步实现,看下教程是怎么指导的
; 先制作一个会移动的点就好
(define (move-worm s)
  (make-world (move-worm-next (world-worm s) Speed)
              (world-food s)))

(define (last-world s)
  (place-image (text "worm hit border" 12 "red")
               (/ BG-width 2) (/ BG-height 2)
               BG))
(define (world-stop? s)
  (not (and
        (<= 0 (posn-x (first (worm-lop (world-worm s)))) BG-width)
        (<= 0 (posn-y (first (worm-lop (world-worm s)))) BG-height))))


(define (place-imagesX img lop bg)
  (cond
    [(empty? lop) bg]
    [else (place-image img
                       (posn-x (first lop)) (posn-y (first lop))
                       (place-imagesX img (rest lop) bg))]))

(define (worm-draw worm bg)
  (place-imagesX Worm (worm-lop worm) bg))
(define (food-draw food bg)
  (place-image Food
               (posn-x food) (posn-y food)
               bg))
(define (world-draw s)
  (worm-draw (world-worm s) (food-draw (world-food s) BG)))

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

(main (make-world (make-worm (list (make-posn 4 15))
                             "right")
                  (make-posn 40 60)))