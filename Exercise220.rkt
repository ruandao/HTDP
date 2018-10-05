;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/itunes)


(define WIDTH 10) ; # of blocks, horizontally 
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))

(define BG (empty-scene SCENE-SIZE SCENE-SIZE))
 
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
               (posn-x lsP) (posn-y lsP)
               bg))
(define (landscape-renders landscape bg)
  (cond
    [(empty? landscape) bg]
    [else
     (landscape-renders (rest landscape)
                        (landscape-render lsp bg))]))

(define (block-render block bg)
  (place-image BLOCK
               (posn-x block) (posn-y block)
               bg))
(define (tetris-render tetris)
  (block-render (tetris-block tetris)
                (landcape-render (tetris-landscape tetris)
                                  BG)))

