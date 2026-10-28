;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-140) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants
;; =========

(define ONLY-BOOLEAN-ERROR-MESSAGE "exception: expects only boolean")


;; Data definitions
;; ================

;; Booleans is one of:
;; - '()
;; - (cons Boolean Booleans)
;; Represents a list of booleans

(define LOB0 '())
(define LOB1 (cons #true '()))
(define LOB2 (cons #false (cons #true '())))

#;
(define (fn-for-booleans booleans)
  (cond [(empty? booleans) (...)]
        [(... (first booleans)) (error ...)]
        [else
         (... (first booleans)
              (fn-for-booleans (rest booleans)))]))

;; Template rules used:
;; - one of 2 cases:
;; - '()
;; - compound: (cons Boolean Booleans)
;; - self-reference: (rest booleans) is Booleans

;; Functions
;; =========

;; Booleans -> Boolean
;; Determines of all values of list is true
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(check-error (all-true (cons "a" '())) ONLY-BOOLEAN-ERROR-MESSAGE)

;(define (all-true booleans) #false)

(define (all-true booleans)
  (cond [(empty? booleans) #true]
        [(not (boolean? (first booleans))) (error ONLY-BOOLEAN-ERROR-MESSAGE)]
        [else
         (and (first booleans)
              (all-true (rest booleans)))]))

;; Booleans -> Boolean
;; Determines if contains one true value
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-error (one-true (cons "a" '())) ONLY-BOOLEAN-ERROR-MESSAGE)

;(define (one-true booleans) #false)

(define (one-true booleans)
  (cond [(empty? booleans) #false]
        [(not (boolean? (first booleans))) (error ONLY-BOOLEAN-ERROR-MESSAGE)]
        [else
         (or (first booleans)
             (one-true (rest booleans)))]))
