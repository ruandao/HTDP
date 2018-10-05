;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise336) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1 Dir.v1)

; A File.v1 is a String



(define TS
  (list
   (list (list "TS/Libs/Code/hang" "TS/Libs/Code/draw")
         (list "TS/Libs/Docs/read!"))
   (list "TS/Text/part1" "TS/Text/part2" "TS/Text/part3")
   "TS/read!"))

(define (how-many? Dir.v1)
  (cond
    [(empty? Dir.v1) 0]
    [(string? (first Dir.v1))
     (+ 1
        (how-many? (rest Dir.v1)))]
    [else (+ (how-many? (first Dir.v1))
             (how-many? (rest Dir.v1)))]))

;(how-many? TS)


(define-struct dir [name content])

; A Dir.v2 is a structure:
;  (make-dir String LOFD)

; An LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2 LOFD)

; A File.v2 is a String

(define TS2
  (make-dir "TS"
            (list "TS/read!"
                  (make-dir "TS/Text" (list "TS/Text/part1"
                                            "TS/Text/part2"
                                            "TS/Text/part3"))
                  (make-dir "TS/Libs" (list
                                       (make-dir "TS/Libs/Code" (list "TS/Libs/Code/hang" "TS/Libs/Code/draw"))
                                       (make-dir "TS/Libs/Docs" (list "TS/Libs/Docs/read!")))))))
(define (how-many-in-dir.v2? Dir.v2)
  (how-many-LOFD.v2? (dir-content Dir.v2)))
(define (how-many-LOFD.v2? LOFD)
  (cond
    [(empty? LOFD) 0]
    [(string? (first LOFD))
     (+ 1
        (how-many-LOFD.v2? (rest LOFD)))]
    [(dir? (first LOFD))
     (+ (how-many-in-dir.v2? (first LOFD))
        (how-many-LOFD.v2? (rest LOFD)))]))

;(how-many-in-dir.v2? TS2)

(define-struct perm [owner group other])
(define-struct file.v3 [name size content])
; A File.v3 is a structure:
;  (make-file String N String)

(define-struct dir.v3 [name dirs files])

(define read10 (make-file.v3 "read!" 10 ""))
(define part1 (make-file.v3 "part1" 99 ""))
(define part2 (make-file.v3 "part2" 52 ""))
(define part3 (make-file.v3 "part3" 17 ""))
(define dText (make-dir.v3 "Text" '() (list part1 part2 part3)))
(define hang (make-file.v3 "hang" 8 ""))
(define draw (make-file.v3 "draw" 2 ""))
(define dCode (make-dir.v3 "Code" '() (list hang draw)))
(define read19 (make-file.v3 "read!" 19 ""))
(define dDocs (make-dir.v3 "Docs" '() (list read19)))
(define dLibs (make-dir.v3 "Libs" (list dCode dDocs) '()))
(define TS3 (make-dir.v3 "TS" (list dText dLibs) (list read10)))


(define (reduce l base f)
  (cond
    [(empty? l) base]
    (else (f (first l)
             (reduce (rest l) base f)))))
(define (reduce-count dir.v3 sum)
  (+ (how-many-files-in-dir.v3? dir.v3)
     sum))
(define (how-many-files-in-dir.v3? dir.v3)
  (+ (reduce (dir.v3-dirs dir.v3) 0 reduce-count)
     (length (dir.v3-files dir.v3))))

;(how-many-files-in-dir.v3? TS3)

(define (reduce-dir-size dir y)
  (+ (how-large-dir.v3? dir)
     y))
(define (reduce-file-size file y)
  (+ (file.v3-size file)
     y))
(define (how-large-dir.v3? dir.v3)
  (+ (reduce (dir.v3-dirs dir.v3) 0 reduce-dir-size)
     (reduce (dir.v3-files dir.v3) 0 reduce-file-size)))

(how-large-dir.v3? TS3)