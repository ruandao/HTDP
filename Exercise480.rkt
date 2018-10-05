;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise480) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define QUEENS 8)
; A QP is a structure:
;  (make-posn CI CI)
; A CI is an N in [0, QUEENS).
; interpretation (make-posn r c) denotes the square at
; the r-th row and c-th column

; will threaten each other?
(define (threatening? qp1 qp2)
  (cond
    [(or (cons? qp1) (cons? qp2)) (error "expect posn as argument")]
    [else 
     (or (horizontal-threaten? qp1 qp2)
         (vertical-threaten? qp1 qp2)
         (diagonal-threaten? qp1 qp2))]))

(define (horizontal-threaten? qp1 qp2)
  (equal? (posn-y qp1) (posn-y qp2)))

(define (vertical-threaten? qp1 qp2)
  (equal? (posn-x qp1) (posn-x qp2)))

(define (diagonal-threaten? qp1 qp2)
  (or (same-sum qp1 qp2)
      (subtract-equal? qp1 qp2)))

(define (same-sum p1 p2)
  (equal? (+ (posn-x p1) (posn-y p1))
          (+ (posn-x p2) (posn-y p2))))
(define (subtract-equal? p1 p2)
  (equal? (- (posn-x p1) (posn-x p2))
          (- (posn-y p1) (posn-y p2))))

(define one-pixle 8)
(define (xy-map n)
  (+ (/ one-pixle 2) (* one-pixle (- n 1))))

(define (render-queens n qps img)
  (cond
    [(empty? qps) (n-queens-board n)]
    [else
     (place-image img
                  (xy-map (posn-x (first qps))) (xy-map (posn-y (first qps)))
                  (render-queens n (rest qps) img))]))

(define (n-queens-board n)
  (row n (col n (rectangle one-pixle one-pixle "outline" "red"))))
(define (row n img)
  (cond
    [(equal? n 0) img]
    [else (above img
                 (row (- n 1) img))]))
(define (col n img)
  (cond
    [(equal? n 0) img]
    [else (beside img
                  (col (- n 1) img))]))

;(render-queens 8 (list (make-posn 4 6)) (circle 2 "solid" "blue"))

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem



(check-member-of (arrangement (list 1 2 3))
                 (list (list 1 2 3)
                       (list 1 3 2)
                       (list 2 1 3)
                       (list 2 3 1)
                       (list 3 1 2)
                       (list 3 2 1)))
; l 的全排列
(check-expect (put-before-each-sublist 4 (list (list 3 2)
                                               (list 9 8)))
              (list (list 4 3 2)
                    (list 4 9 8)))
(define (put-before-each-sublist x l)
  (map (lambda (l2) (cons x l2)) l))
(check-expect (unlist-all-item (list (list 3 2)
                                     (list 9 8)))
              (list 3 2 9 8))
(define (unlist-all-item parentList)
  (foldr append '() parentList))
(check-expect (remove 3 (list 2 3 4))
              (list 2 4))
(define (arrangement l)
  (local ((define (f x)
            (local ((define without (remove x l))
                    (define restArr (arrangement without)))
              (put-before-each-sublist x restArr))))
    (cond
      [(empty? l) '(())]
      [else 
       (unlist-all-item
        (map f l))])))



(check-expect (equal-set? (all-possible 1)
                          (list (make-posn 0 0)))
              #true)
(check-expect (equal-set? (all-possible 2)
                          (list (make-posn 0 0)
                                (make-posn 0 1)
                                (make-posn 1 0)
                                (make-posn 1 1)))
              #true)
(define (equal-set? l1 l2)
  (and (andmap (lambda (item)
                 (member? item l1))
               l2)
       (andmap (lambda (item)
                 (member? item l2))
               l1)))

(define (all-item-for n)
  (cond
    [(equal? n 0) (list 0)]
    [else (cons n (all-item-for (- n 1)))]))
(define (1n-map x l)
  (map (lambda (y)
         (list x y))
       l))
(define (nm-map l1 l2)
  (unlist-all-item
   (map (lambda (x)
          (1n-map x l2))
        l1)))
(define (all-possible n)
  (map (lambda (l)
         (make-posn (first l) (second l)))
       (nm-map (all-item-for (- n 1)) (all-item-for (- n 1)))))

(define (exist-with item others)
  (andmap (lambda (x)
            (cond
              [(or (cons? x) (cons? item)) (error "expect posn")]
              [else 
               (threatening? item x)]))
          others))
(check-expect (on/off (list (make-posn 0 0)
                            (make-posn 0 1)
                            (make-posn 1 0)
                            (make-posn 1 1)) 2)
              '())
(check-expect (member? (on/off (list (make-posn 0 0)) 1)
                          (list (list (make-posn 0 0))))
              #true)

(define (not-threatening-with x l)
  (cond
    [(cons? x) (error "expect x as an posn")]
    [(empty? l) '()]
    [else (if (threatening? x (first l))
              (not-threatening-with x (rest l))
              (cons (first l) (not-threatening-with x (rest l))))]))
(check-expect (max-length-items '((1 2 3) (1 2 3 4)))
              '((1 2 3 4)))
(define (max-length-items l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) l]
    [(> (length (first l)) (length (first (max-length-items (rest l)))))
     (list (first l))]
    [(= (length (first l)) (length (first (max-length-items (rest l)))))
     (cons (first l) (max-length-items (rest l)))]
    [else (max-length-items (rest l))]))
; 选择/不选择, 对某些元素进行选择, 看能得到多少种组合
(define (on/off possible n)
  (cond
    [(empty? possible) '()]
    [else
     (local ((define item (first possible))
             (define withoutfirst (rest possible)))
       (cond
         [(cons? item) (error "expect item as an posn")]
         [else
          (local (
                  (define not-threatening-with-item (not-threatening-with item withoutfirst))
                  (define findResultWithFirst (cons item (on/off not-threatening-with-item (- n 1))))
                  (define findResultWithoutFirst (on/off withoutfirst n)))
            (cond
              [(>= (length findResultWithFirst) n) findResultWithFirst]
              [(>= (length findResultWithoutFirst) n) findResultWithoutFirst]
              [else '()]))]))]))

(check-expect (member? (on/off (list (make-posn 0 0)) 1)
                       (list (list (make-posn 0 0))))
              #true)
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list (make-posn 0 2) (make-posn 1 0)
        (make-posn 2 3) (make-posn 3 1)))

(define 0-1 (make-posn 0 1))
(define 1-3 (make-posn 1 3))
(define 2-0 (make-posn 2 0))
(define 3-2 (make-posn 3 2))

(define (sub-set-of? set1 set2)
  (andmap (lambda(x)
            (member? x set2))
          set1))

(check-expect  (member? (n-queens 4)
                        (arrangement (list 0-1 1-3 2-0 3-2)))
               #true)
(define (n-queens n)
  (local ((define all-posn (all-possible n))
          (define availables (on/off all-posn n)))
    (if (equal? (length availables) 0)
        #false
         availables)))

(time (n-queens 4))
(time (n-queens 5))
(time (n-queens 6))
(time (n-queens 7))
(time (n-queens 8))