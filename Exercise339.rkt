;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exercise339) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require htdp/dir)

(define (reduce-dir-count dir y)
  (+ (how-many? dir)
     y))
(define (reduce l base f)
  (cond
    [(empty? l) base]
    [else (f (first l)
             (reduce (rest l) base f))]))
(define (how-many? dir)
  (+ (reduce (dir-dirs dir) 0 reduce-dir-count)
     (length (dir-files dir))))

(define o (create-dir "/Users/weixin/tmp"))

;(how-many? o)
(define (find-file-in-list? fname l)
  (cond
    [(empty? l) #false]
    [else (or (equal? fname (file-name (first l)))
              (find-file-in-list? fname (rest l)))]))
(define (find? dir fname)
  (or (find-file-in-list? fname (dir-files dir))
      (reduce (dir-dirs dir)
              #false
              (lambda (d y)
                (or y
                    (find? d fname)))
              )))

(find? o "Exercise103.rkt")