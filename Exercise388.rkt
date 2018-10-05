;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise388) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front)
           (replace-eol-with (rest front) end))]))

(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross los lon)
  (local ((define (reduce l empty-case f)
            (cond
              [(empty? l) empty-case]
              [else (f (first l)
                       (reduce (rest l) empty-case f))]))
          (define mResult
            (map (lambda (s)
                   (map (lambda (n) (list s n))
                        lon))
                 los)))
    (reduce mResult '() replace-eol-with)))

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on
; hours and wages/h
; assume the two lists are of equal length
(define (wages*.v2 employees work-records)
  (cond
    [(empty? employees) '()]
    [else
     (cons
      (weekly-wage (first employees) (first work-records))
      (wages*.v2 (rest employees) (rest work-records)))]))
(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define-struct employee [ name ssn pay-rate])
(define-struct work-record [name hours])
; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage employee work-record)
  (* (employee-pay-rate employee) (work-record-hours work-record)))

