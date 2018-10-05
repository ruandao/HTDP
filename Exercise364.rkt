;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Exercise364) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; - (cons Symbol Body)
; - (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '())
; so Body is:
; - '()
; - (cons Xexpr.v2 Body)

; <transition from="seen-e" to="seen-f" />
(list transition
      (list (list from "seen-e")
            (list to "seen-f"))
      '())
; <ul><li><word /><word /></li><li><word /></li></ul>
(list ul
      '()
      (list li
            '()
            (list (list word '())
                  (list word '())))
      (list li
            '()
            (list (list word '()))))

'(server ((name "example.org")))
;<server name="example.org" />
'(carcas (board (grass)) (player ((name "sam"))))
<carcas>
  <board>
    <grass/>
  </board>
  <player name="sam" />
</carcas>

'(start)
<start />



