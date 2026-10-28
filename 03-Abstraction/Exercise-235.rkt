;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-235) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ================
;; Data definitions

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; Represents a list contains string.

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions
;; =========

;; String ListOfString -> Boolean
;; determines if l contains s.
(check-expect (contains? "a" empty) #false)
(check-expect (contains? "a" (list "a" "b")) #true)
(check-expect (contains? "c" (list "a" "b" "c")) #true)
(check-expect (contains? "a" (list "b" "c")) #false)

#;
(define (contains? s l) #false)

(define (contains? s los)
  (cond [(empty? los) #false]
        [else
         (or (string=? s (first los))
             (contains? s (rest los)))]))

;; ListOfString -> Boolean
;; determines if l contains "atom".

(define (contains-atom? l)
  (contains? "atom" l))

;; ListOfString -> Boolean
;; determines if l contains "basic".

(define (contains-basic? l)
  (contains? "basic" l))

;; ListOfString -> Boolean
;; determines if l contains "zoo".

(define (contains-zoo? l)
  (contains? "zoo" l))