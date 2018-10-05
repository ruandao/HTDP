;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise217) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)


; 这个贪吃蛇的设计,是将贪吃蛇的每个片段,当做一个可以移动的点
; 每次变向,就将变向的点,加入到片段的变向点集
; 当一个片段经过变向点集时,就改变方向,并将该变向点从点集中移除
; 每次移动,都将片段往片段的方向移动一格(Speed)


(define BG-width 300)
(define BG-height 300)
(define BG (empty-scene BG-width BG-height))
(define Speed 1)
(define Worm [circle 3 "solid" "red"])
(define Food [rectangle 2 2 "outline" "green"])

(define-struct world [ worm food ])

(define-struct cPoint [ x y direction])
; 每个片段在 cPoint 的x,y坐标, 方向改变为 cPoint-direction
; 经过改变点后,就改变方向,然后把当前的改变点从cps 中移除
; cps 表示片段需要变向的点的list
(define-struct segment [ x y direction cps])
; An Worm is an:
; - (cons segment '())
; - (cons segment Worm)

(define (add-to-end l p)
  (cond
    [(empty? l) (cons p '())]
    [else (cons (first l)
                (add-to-end (rest l) p))]))
; 给每个蠕虫片段添加变向点
(define (change-segments-direction los cPoint)
  (cond
    [(empty? los) '()]
    [else (cons (make-segment (segment-x (first los))
                              (segment-y (first los))
                              (segment-direction (first los))
                              (add-to-end (segment-cps (first los)) cPoint))
                (change-segments-direction (rest los) cPoint))]))
; 根据蠕虫的head和方向,建立变向点
(define (cPoint-from-head segment direction)
  (make-cPoint (segment-x segment) (segment-y segment) direction))
; 修改蠕虫的方向
(define (change-worm-direction worm direction)
  (change-segments-direction worm (cPoint-from-head (first worm) direction)))
; 改变蠕虫的方向
(define (make-worm->left world)
  (make-world (change-worm-direction (world-worm world)
                                     "left")
              (world-food world)))
(define (make-worm->right world)
  (make-world (change-worm-direction (world-worm world)
                                     "right")
              (world-food world)))
(define (make-worm->up world)
  (make-world (change-worm-direction (world-worm world)
                                     "up")
              (world-food world)))
(define (make-worm->down world)
  (make-world (change-worm-direction (world-worm world)
                                     "down")
              (world-food world)))


; 惯性移动
(define (inertia-move s Speed)
  (cond
    [(equal? (segment-direction s) "left")
     (make-segment (- (segment-x s) Speed)
                   (segment-y s)
                   (segment-direction s)
                   (segment-cps s))]
    [(equal? (segment-direction s) "right")
     (make-segment (+ (segment-x s) Speed)
                   (segment-y s)
                   (segment-direction s)
                   (segment-cps s))]
    [(equal? (segment-direction s) "up")
     (make-segment (segment-x s)
                   (- (segment-y s) Speed)
                   (segment-direction s)
                   (segment-cps s))]
    [(equal? (segment-direction s) "down")
     (make-segment (segment-x s)
                   (+ (segment-y s) Speed)
                   (segment-direction s)
                   (segment-cps s))]))
; 检测片段和变向点是否在同一个点
(define (segment-cp-at-same-point s cp)
  (and (equal? (segment-x s) (cPoint-x cp))
       (equal? (segment-y s) (cPoint-y cp))))
; 将片段往前移动Speed
(define (move-segment-next s Speed)
  (cond
    [(empty? (segment-cps s)) (inertia-move s Speed)]
    [else (if (segment-cp-at-same-point s (first (segment-cps s)))
              (move-segment-next (make-segment (segment-x s)
                                               (segment-y s)
                                               (cPoint-direction (first (segment-cps s)))
                                               (rest (segment-cps s)))
                                 Speed)
              (inertia-move s Speed))]))
; 移动蠕虫
(define (move-worm-next worm Speed)
  (cond
    [(empty? worm) '()]
    [else (cons (move-segment-next (first worm) Speed)
                (move-worm-next (rest worm) Speed))]))
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
        (<= 0 (segment-x (first (world-worm s))) BG-width)
        (<= 0 (segment-y (first (world-worm s))) BG-height))))

(define (segment-draw s bg)
  (place-image Worm
               (segment-x s) (segment-y s)
               bg))
(define (worm-draw worm bg)
  (cond
    [(empty? worm) bg]
    [else (worm-draw (rest worm)
                     (segment-draw (first worm) bg))]))
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

(main (make-world (cons (make-segment 4 15 "right" '()) '())
                  (make-posn 40 60)))