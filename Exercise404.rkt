;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise404) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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


(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;  (make-phone-record String String)
(define (zip lon lopn)
  (cond
    [(empty? lon) '()]
    [else
     (cons (make-phone-record (first lon) (first lopn))
           (zip (rest lon) (rest lopn)))]))

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
  (cond
    [(and (= n 0) (empty? l))
     (error 'list-pick "list too short")]
    [(and (> n 0) (empty? l))
     (error 'list-pick "list too short")]
    [(and (= n 0) (cons? l)) (first l)]
    [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))

(define-struct branch [left right])

; A TOS is one of:
; - Symbol
; - (make-branch TOS TOS)

; A Direction is one of:
; - 'left
; - 'right

; A list of Directions is also called a path.

; TOS [List-of Direction] -> TOS
(define (tree-pick tos lod)
  (cond
    [(and (empty? lod) (branch? tos))
     tos]
    [(and (empty? lod) (symbol? tos))
     tos]
    [(and (symbol? tos) (cons? lod))
     (error "too depth path")]
    [(and (branch? tos) (cons? lod))
     (cond
       [(equal? (first lod) 'left)
        (tree-pick (branch-left tos) (rest lod))]
       [(equal? (first lod) 'right)
        (tree-pick (branch-right tos) (rest lod))])]))

(define-struct db [schema content])
; A DB is a structure:
;  (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Table is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions

; integrity constraint In (make-db sch con),
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"  ,string?)
    ("Age"   ,integer?)
    ("Present" ,boolean?)))
(define school-content
  `(("Alice" 25 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave" 32 #false)))

(define school-db
  (make-db school-schema
           school-content))

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)

(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)

(define (integrity-check db)
  (integrity-check-rows (school-db-school-schema db)
                        (school-db-school-content db)))
(define (integrity-check-rows schema rows)
  (cond
    [(empty? rows) #true]
    [else (and (integrity-check-row schema (first rows))
               (integrity-check-rows schema (rest rows)))]))
(define (integrity-check-row schema row)
  [(and (empty? schema) (empty? row)) #true]
  [(or (empty? schema) (empty? row)) #false]
  [else (and ((spec-predicate (first schema)) (first row))
             (integrity-check-row (rest schema) (rest row)))])

(define (andmap2 f l1 l2)
  (cond
    [(empty? l1) #true]
    [else (and (f (first l1) (first l2))
               (andmap2 f (rest l1) (rest l2)))]))
