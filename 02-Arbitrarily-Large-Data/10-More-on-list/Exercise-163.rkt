;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-163) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; Represents a list of number

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Functions
;; =========

;; ListOfNumber -> ListOfNumber
;; Converts a list of measurements in Fahreherit
(check-expect (lof->loc empty) empty)
(check-within (lof->loc (list 0 32 212)) (list -17.78 0 100) 0.01)

;(define (fahrenheit->celsius lon) lon)

(define (lof->loc lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fahrenheit->celsius (first lon))
               (lof->loc (rest lon)))]))

;; Number -> Number
;; Convers fahrenheit to celsius
(check-within (fahrenheit->celsius 0) -17.78 0.01)
(check-within (fahrenheit->celsius 32) 0 0.01)
(check-within (fahrenheit->celsius 212) 100 0.01)

;(define (fahrenheit->celsius n) n)

(define (fahrenheit->celsius f)
  (/ (- f 32) 1.8))
