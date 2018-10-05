;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise372) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; - (cons Symbol [List-of Xexpr.v2])
; - (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '())

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) a0)
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) a0)
(define (xexpr-attr xe)
  (cond
    [(is-body? (rest xe)) '()] ; (cons Symbol Body)
    [else (second xe)] ; (cons Symbol (cons [List-of Attribute] Body)
    ))
(define (is-body? attr-or-body)
  (cond
    [(empty? attr-or-body) #true] ; '()
    [(empty? (first attr-or-body)) #false]
    [(symbol? (first (first attr-or-body))) #true] ; (cons Xexpr.v2 Body)
    [else #false] ; (not symbol? (first (first attr-or-body))) is attr
    ))

(define (xexpr-name xe)
  (first xe))
(define (xexpr-content xe)
  (cond
    [(is-body? (rest xe))
     (rest xe)]
    [else (rest(rest xe))]))

(check-expect (find-attr a0 'initial) "X")
(define (find-attr loa s)
  (local ((define result (assq s loa)))
    (if (boolean? result)
        result
        (second result))))

; An XWord is '(word ((text String))).
(define (word? sexp)
  (equal? (first sexp) 'word))
(define w1
  '(word ((text "nnn"))))
(check-expect (word-text w1) "nnn")
(define (word-text word)
  (find-attr (second word) 'text))

; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons Attributes [List-of XItem.v1])
; An XItem.v1 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons Attributes (cons XWord '())))

(define BT (circle 3 "solid" "red"))
(define xe0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define xe0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

(define item1
    '(li (word ((text "one")))))
(define item1-rendered
  (beside/align 'center BT (text "one" 12 'black)))


; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 item1) item1-rendered)
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

; XEnum.v1 -> Image
; renders a simple enumeration as an image
(check-expect (render-enum1 xe0) xe0-rendered)
(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left
                         (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
; 
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))
