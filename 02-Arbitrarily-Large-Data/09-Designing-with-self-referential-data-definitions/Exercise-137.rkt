;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-137) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; ListOfString is one of:
;; - '()
;; - (cons String ListOfString)
;; Represents a list of string

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)                       ;String
              (fn-for-los (rest los)))]))       ;ListOfString

;; Template rules used:
;; - one of 2 cases:
;; - atomic distinct: '()
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

;; Functions
;; =========

;; ListOfString -> Number
;; Produce size of list
(check-expect (how-many empty) 0)
(check-expect (how-many (cons "a" '())) 1)
(check-expect (how-many (cons "a" (cons "b" '()))) 2)

;(define (how-many los) 0)

(define (how-many los)
  (cond [(empty? los) 0]
        [else
         (+ 1
            (how-many (rest los)))]))


;; ListOfString -> Boolean
;; Determines if found "Flatt" in list
(check-expect (contains-flatt empty) #false)
(check-expect (contains-flatt (cons "Flatt" '())) #true)
(check-expect (contains-flatt (cons "a" (cons "Flatt" '()))) #true)
(check-expect (contains-flatt (cons "a" (cons "b" '()))) #false)

;(define (contains-flatt los) #false)

(define (contains-flatt los)
  (cond [(empty? los) #false]
        [else
         (or (string=? (first los) "Flatt")
             (contains-flatt (rest los)))]))


;; Question
;; ========

;;Compare the template for contains-flatt? with the one for how-many

;; Answer
;; ======

;; Templates are built based on a data definition
;; Both of these templates deal with the self-referential List data definition
;; Hence structures of the templates are similar
