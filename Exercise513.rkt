;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise513) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

(define (is-var? x)
  (symbol? x))

(define (is-λ? x)
  (and (cons? x)
       (equal? 'λ (first x))))
(define (is-app? x)
  (and (not (is-var? x))
       (not (is-λ? x))
       (and (cons? x)
            (equal? 2 (length x)))))

(define (λ-para x)
  (second x))
(define (λ-body x)
  (third x))

(define (app-fun x)
  (first x))
(define (app-arg x)
  (second x))

; Lam -> Lam
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ
; expression whose parameter is s

(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)

(define (undeclareds le0)
  (local ((define (u/a le0 env)
            (cond
              [(is-var? le0)
               (if (find le0 env)
                   le0
                   '*undeclared)]
              [(is-λ? le0)
               (list 'λ (λ-para le0)
                     (u/a (λ-body le0) (append (λ-para le0) env)))]
              [(is-app? le0)
               (list (u/a (app-fun le0) env) (u/a (app-arg le0) env))])))
    (u/a le0 '())))
(define (find x env)
  (cond
    [(empty? env) #false]
    [else (or (equal? x (first env))
              (find x (rest env)))]))

; Lam -> Lam
(define (undeclaredsX le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ
          ; parameters on the path from le0 to le
          (define (u/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds) le (list '*undeclared le))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (u/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (u/a fun declareds)
                       (u/a arg declareds)))])))
    (u/a le0 '())))