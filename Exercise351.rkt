;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise351) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct add [left right])
(define-struct mul [left right])

(define (eval-expression exp)
  (cond
    [(number? exp) exp]
    [(equal? '+ (first exp))
     (+ (eval-expression (second exp))
        (eval-expression (third exp)))]
    [(equal? '* (first exp))
     (* (eval-expression (second exp))
        (eval-expression (third exp)))]))

(eval-expression '(+ 1 1))
(eval-expression '(+ (* 3 3) (* 4 4)))

(define WRONG "something error")
(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

(define (eval x)
  (cond
    [(number? x) x]
    [(add? x)
     (+ (eval (add-left x))
        (eval (add-right x)))]
    [(mul? x)
     (* (eval (mul-left x))
        (eval (mul-right x)))]))
(eval (parse '(+ (* 3 3) (* 4 4))))
