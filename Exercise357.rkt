;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise357) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
    [(symbol? s) s]))

(define (find-var x al)
  (cond
    [(empty? al) (error (string-append "not found var:" x))]
    [(equal? (first (first al)) x)
     (second (first al))]
    [else (find-var x (rest al))]))
(define (eval x al)
  (cond
    [(number? x) x]
    [(symbol? x) (find-var x al)]
    [(add? x)
     (+ (eval (add-left x) al)
        (eval (add-right x) al))]
    [(mul? x)
     (* (eval (mul-left x) al)
        (eval (mul-right x) al))]))
;(eval (parse '(+ (* 3 3) (* 4 4))))

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (cond
       [(equal? ex x) v]
       [else ex])]
    [(add? ex)
     (make-add (subst (add-left ex) x v)
               (subst (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst (mul-left ex) x v)
               (subst (mul-right ex) x v))]))

;(subst (parse '(+ (* x 3) (* x 4))) 'x 5)
(define as-x (list 'x 9))
(define al (list as-x))
(eval (parse '(+ (* x 3) (* x 4))) al)
; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;  (cons Symbol (cons Number '())).






