;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise221) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)


(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 10)
(define SIZE 10) ; blocks are squares
(define SIZE/2 (/ SIZE 2))
(define SCENE-width (* WIDTH SIZE))
(define SCENE-height (* HEIGHT SIZE))

(define BG (empty-scene SCENE-width SCENE-height))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

;(define landscape0 ...)
;(define block-dropping ...)
;(define tetris0 ...)
;(define tetris0-drop ...)

;(define block-landed (make-block 0 (- HEIGHT 1)))
;(define block-on-block (make-block 0 (- HEIGHT 2)))

; render
(define (landscape-render lsP bg)
  (place-image BLOCK
               (+ SIZE/2 (* SIZE (block-x lsP)))
               (+ SIZE/2 (* SIZE (block-y lsP)))
               bg))
(define (landscape-renders landscape bg)
  (cond
    [(empty? landscape) bg]
    [else
     (landscape-renders (rest landscape)
                        (landscape-render (first landscape) bg))]))

(define (block-render block bg)
  (place-image BLOCK
               (+ SIZE/2 (* SIZE (block-x block)))
               (+ SIZE/2 (* SIZE (block-y block)))
               bg))
(define (tetris-render tetris)
  (block-render (tetris-block tetris)
                (landscape-renders (tetris-landscape tetris)
                                  BG)))


; tick
(define (tetris-block->landscape w)
  (cons (tetris-block w) (tetris-landscape w)))
(define (landed-block? b)
  (>= (block-y b) HEIGHT))
(define (conflict? w)
  (or (member? (tetris-block w) (tetris-landscape w))
      (landed-block? (tetris-block w))))
(define (block-drop block)
  (make-block (block-x block) (+ (block-y block) 1)))
(define (tetris-block-drop w)
  (make-tetris (block-drop (tetris-block w))
               (tetris-landscape w)))
(define (tetris-tick w)
  (if (conflict? (tetris-block-drop w))
      (make-tetris (block-generate -1)
                   (tetris-block->landscape w))
      (tetris-block-drop w)))
; key
(define (move-block-right block)
  (cond
    [(equal? (block-x block) (- WIDTH 1)) block]
    [else (make-block (+ (block-x block) 1)
                      (block-y block))]))
(define (move-tetris-block-right w)
  (make-tetris (move-block-right (tetris-block w))
               (tetris-landscape w)))
(define (move-block-left block)
  (cond
    [(equal? (block-x block) 0) block]
    [else (make-block (- (block-x block) 1)
                      (block-y block))]))
(define (move-tetris-block-left w)
  (make-tetris (move-block-left (tetris-block w))
               (tetris-landscape w)))
(define (tetris-key w ke)
  (cond
    [(equal? ke "left") (move-tetris-block-left w)]
    [(equal? ke "right") (move-tetris-block-right w)]
    [else w]))


(check-expect (equal? (block-generate -1)
                      (make-block -1 0))
              #false)
(define (block-generate x)
  (block-generate-not (random WIDTH) x))
(check-expect (block-generate-not 2 1)
              (make-block 2 0))
(define (block-generate-not x1 x)
  (if (equal? x1 x)
      (block-generate x)
      (make-block x1 0)))
(define (tetris-main rate)
  (big-bang (make-tetris (block-generate -1) '())
    [on-tick tetris-tick rate]
    [on-draw tetris-render]
    [on-key tetris-key]))


(tetris-main (/ 1 3))