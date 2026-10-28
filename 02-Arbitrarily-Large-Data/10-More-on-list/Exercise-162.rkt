;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-162) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


;; Constants
;; =========

(define RATE 14)
(define MAX-HOURS 100)
(define HOURS-ERROR
  (string-append "wage*: hours cannot exceed " (number->string MAX-HOURS)))

;; Functions
;; =========

;; ListOfNumber -> ListOfNumber
;; Produce the weekly wages for all given weekly hours.
(check-expect (wage* empty) empty)
(check-expect (wage* (list 28)) (list (* 28 RATE)))
(check-expect (wage* (list 28 20)) (list (* 28 RATE) (* 20 RATE)))
(check-error (wage* (list 28 101)) HOURS-ERROR)

;(define (wage* lon) empty)

(define (wage* lon)
  (cond [(empty? lon) empty]
        [(> (first lon) MAX-HOURS) (error HOURS-ERROR)]
        [else
         (cons (wage (first lon))
               (wage* (rest lon)))]))

;; Number -> Number
;; Produce weekly wage for weekly hours
(check-expect (wage 0) 0)
(check-expect (wage 20) (* 20 RATE))

;(define (wage n) 0)

(define (wage n)
  (* RATE n))
