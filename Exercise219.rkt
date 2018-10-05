;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise219) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)


; 这个贪吃蛇的设计, 每次移动,都在头部加上一个点,然后去除尾部的点

(define BG-width 300)
(define BG-height 300)
(define BG (empty-scene BG-width BG-height))
(define Speed 1)
(define Worm [rectangle 2 2 "solid" "red"])
(define Food [rectangle 2 2 "outline" "green"])
(define MAX BG-width)

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




(define (new-head s direction Speed)
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

(define (add-to-tail l p)
  (cond
    [(empty? l) (list p)]
    [else (cons (first l)
                (add-to-tail (rest l) p))]))
; 如果需要加入尾部,并不需要加一个具体的位置,
; 只需要一个表示位置的东西,下一次有东西扣减即可
(define (add-new-tail-if-need l newhead food)
  (if (equal? newhead food)
      (add-to-tail l food)
      l))
(define (remove-last l)
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l)
                (remove-last (rest l)))]))
(define (add-to-head l s)
  (cons s l))
; 移动蠕虫
(define (move-worm-next worm Speed)
  (make-worm (add-to-head (remove-last (worm-lop worm))
                          (new-head (first (worm-lop worm))
                                    (worm-direction worm)
                                    Speed))
             (worm-direction worm)))
(define (eated-food worm food)
  (make-worm (add-to-tail (worm-lop worm)
                          food)
             (worm-direction worm)))
(define (eated? worm food)
  (equal? (first (worm-lop worm))
          food))
(define (create-food-not p)
  (create-food-random p (make-posn (random MAX) (random MAX))))
(define (create-food-random p candidate)
  (if (equal? p candidate)
      (create-food-not p)
      candidate))
; 要考虑是否和食物接触到,接触到,需要产生下一个食物, 并且增长
; ... 先放弃上面的想法,跟着教程,一步步实现,看下教程是怎么指导的
; 先制作一个会移动的点就好
(define (move-worm s)
  (cond
    [(eated? (move-worm-next (world-worm s) Speed)
             (world-food s))
     (make-world (eated-food (move-worm-next (world-worm s) Speed)
                             (world-food s))
                 (create-food-not (make-posn 1 1)))]
    [else
     (make-world (move-worm-next (world-worm s) Speed)
                 (world-food s))]))

(define (last-world-text content)
  (place-image (text content 12 "red")
               (/ BG-width 2) (/ BG-height 2)
               BG))
(define (last-world s)
  (cond
    [(run-into-wall? (world-worm s)) (last-world-text "worm hit border")]
    [(run-into-itself? (world-worm s)) (last-world-text "worm hit itself")]
    [else (error (world-worm s))]))
  
; 碰到墙
(check-expect (run-into-wall? (make-worm (list (make-posn 4 -1)) "up"))
              #true)
(check-expect (run-into-wall? (make-worm (list (make-posn 3 3)) "up"))
              #false)
(define (run-into-wall? worm)
  (not (and
        (<= 0 (posn-x (first (worm-lop worm))) BG-width)
        (<= 0 (posn-y (first (worm-lop worm))) BG-height))))

; EmptyOrOne
; - '()
; - (cons p '())

; l
; - EmptyOrOne
; - (cons p l)
(define (last l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) (first l)]
    [else (last (rest l))]))
; 蠕虫是否碰到自己
(define (run-into-itself? worm)
  (cond
    [(equal? (first (worm-lop worm))
             (last (rest (worm-lop worm))))
     (member? (first (worm-lop worm))
              (remove-last (rest (worm-lop worm))))]
    [else
     (member? (first (worm-lop worm)) (rest (worm-lop worm)))]))
  
; 是否结束世界
(define (world-stop? s)
  (or (run-into-wall? (world-worm s))
           (run-into-itself? (world-worm s))))


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
  (length (big-bang s
            [on-draw world-draw]
            [on-tick world-tick]
            [stop-when world-stop? last-world]
            [on-key world-key])))

(main (make-world (make-worm (list (make-posn 4 15))
                             "right")
                  (make-posn 40 60)))



; Posn -> Posn
; ???
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create p (make-posn (random MAX) (random MAX))))

; Posn Posn -> Posn
; generative recursion
; ???
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1)
            (= (posn-y p) 1))))