;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise508) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; [List-of 1String] -> Image
; renders a string as an image for the editor
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

(define-struct editor [pre post])
; An Editor is a structure:
;  (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

(check-expect (split-structural (create-editor "dklf" "dkfl") 0)
              (create-editor "" "dklfdkfl"))
(check-expect (image-width (editor-text '()))
              0)
(check-expect (image-width (editor-text (list "a")))
              9)
(check-expect (image-width (editor-text (list "a" "b")))
              18)
(check-expect (isAvailableSplit (list "a" "b") (list "c" "d") 5)
              #false)
(check-expect (isAvailableSplit (list "a" "b") (list "c" "d") 20)
              #true)
(define (isAvailableSplit p s x)
            (<= (image-width (editor-text p))
                x
                (+ (image-width CURSOR)
                   (image-width (editor-text (append p (list (first s))))))))
(define (try p s x)
  (cond
    [(empty? s) (make-editor p s)]
    [else
     (if (isAvailableSplit p s x)
         (make-editor p s)
         (try (append p (list (first s))) (rest s) x))]))
(try '("a" "b") '("c" "d") 0)
(isAvailableSplit '("a" "b") '("c" "d") 0)
(define (split-structural ed x)
  (try '() (append (editor-pre ed) (editor-post ed)) x))


; Lo1s -> Lo1s
; produces a reverse version of the given list

(check-expect
 (rev (cons "a" (cons "b" (cons "c" '()))))
 (cons "c" (cons "b" (cons "a" '()))))
(define (rev l)
  (local ((define (rev/a l result)
            (cond
              [(empty? l) result]
              [else (rev/a (rest l) (cons (first l) result))])))
    (rev/a l '())))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l

(check-expect
 (add-at-end (cons "c" (cons "b" '())) "a")
 (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l s)
  (cond
    [(empty? l) (list s)]
    [else (cons (first l) (add-at-end (rest l) s))]))


(define (create-editor p s)
  (make-editor (explode p)  (explode s)))



; Editor -> Image
; renders an editor as an image of the two texts
; separated by the cursor
(define (editor-render e)
  (place-image/align
   (beside (text (implode (editor-pre e)) FONT-SIZE FONT-COLOR)
           CURSOR
           (text (implode (editor-post e)) FONT-SIZE FONT-COLOR))
   1 1
   "left" "top"
   MT))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
 (editor-kh (create-editor "cd" "fgh") "e")
 (create-editor "cde" "fgh"))
(check-expect
 (editor-kh (create-editor "abcd" "wert") "\b")
 (create-editor "abc" "wert"))
(define (editor-kh ed ke)
  (cond
    [(equal? ke "left") (move-curson-left ed)]
    [(equal? ke "right") (move-curson-right ed)]
    [(equal? ke "\b") (delete-curson-left ed)]
    [else (insert-left ed ke)]))

(check-expect (move-curson-left (create-editor "dkf" "kdfk"))
              (create-editor "dk" "fkdfk"))
(define (move-curson-left ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else (make-editor (remove-last (editor-pre ed))
                       (add-to-first (last (editor-pre ed)) (editor-post ed)))]))
(define (last nel)
  (cond
    [(empty? (rest nel)) (first nel)]
    [else (last (rest nel))]))
(check-expect (remove-last (list "k" "m"))
              (list "k"))
(define (remove-last nel)
  (cond
    [(empty? (rest nel)) '()]
    [else (cons (first nel) (remove-last (rest nel)))]))
(define (add-to-first x l)
  (cons x l))
(define (move-curson-right ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else (make-editor (add-to-last (first (editor-post ed)) (editor-pre ed))
                       (remove-first (editor-post ed)))]))
(define (add-to-last x l)
  (cond
    [(empty? l) (list x)]
    [else (cons (first l) (add-to-last x (rest l)))]))
(define (remove-first nel)
  (rest nel))

(define (insert-left ed ke)
  (make-editor (add-to-last ke (editor-pre ed))
               (editor-post ed)))

(define (delete-curson-left ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else (make-editor (remove-last (editor-pre ed))
                       (editor-post ed))]))
;(index "editor-kh")

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
    [on-key editor-kh]
    [to-draw editor-render]
    [on-mouse editor-mouse]))

(define (editor-mouse ed x y e)
  (split-structural ed x))

(main "dklfsldfs")