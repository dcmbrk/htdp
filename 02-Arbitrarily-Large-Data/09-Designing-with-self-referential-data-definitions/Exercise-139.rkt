;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-139) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants
;; =========

(define NUMBER-ONLY-ERROR-MESSAGE
  "function expects a list of numbers")

(define NON-NEGATIVE-NUMBER-ONLY-ERROR-MESSAGE
  "function expects a list of non-negative numbers")

;; Data definitions
;; ================

;; Numbers is one of:
;; - '()
;; - (cons Number Numbers)
;; Represents a list of numbers

(define N0 '())
(define N1 (cons 1 '()))
(define N2 (cons 2 (cons 1 '())))

#;
(define (fn-for-numbers numbers)
  (cond [(empty? numbers) (...)]
        ;;[(exception? (fist numbers)) (...)]
        [else
         (... (first numbers)                     ;Number
              (fn-for-numbers (rest numbers)))])) ;Numbers

;; Template rules used:
;; - one of 2 cases:
;; - atomic distinct: Number
;; - compound: (cons Number Numbers)
;; - self-reference: (rest numbers) is Numbers

;; Functions
;; =========


;; Numbers -> Boolean
;; Determines if numbers only positive number, true if empty list

(check-error  (positive-numbers? (cons "a" '())) NUMBER-ONLY-ERROR-MESSAGE)
(check-error  (positive-numbers? (cons "a" (cons "b" '()))) NUMBER-ONLY-ERROR-MESSAGE)
(check-error  (positive-numbers? (cons 1 (cons "b" '()))) NUMBER-ONLY-ERROR-MESSAGE)
(check-expect (positive-numbers? (cons -1 (cons "b" '()))) #false)
(check-expect (positive-numbers? '()) #true)
(check-expect (positive-numbers? '()) #true)
(check-expect (positive-numbers? (cons 1 '())) #true)
(check-expect (positive-numbers? (cons 1 (cons 2 '()))) #true)
(check-expect (positive-numbers? (cons -1 '())) #false)
(check-expect (positive-numbers? (cons 1 (cons -2 '()))) #false)

;(define (positive-numbers? numbers) #false)

(define (positive-numbers? numbers)
  (cond [(empty? numbers) #true]
        [(not (number? (first numbers))) (error NUMBER-ONLY-ERROR-MESSAGE)]
        [else
         (and (positive? (first numbers))                     ;Number
              (positive-numbers? (rest numbers)))]))          ;Numbers


;; Numbers -> Number
;; Produce sum of non-negative numbers

(check-expect (total-non-negative-numbers '()) 0)
(check-expect (total-non-negative-numbers (cons 1 '())) (+ 1 0))
(check-expect (total-non-negative-numbers (cons 2 (cons 1 '()))) (+ 2 1 0))
(check-error (total-non-negative-numbers (cons "a" '())) NUMBER-ONLY-ERROR-MESSAGE)
(check-error (total-non-negative-numbers (cons -1 '())) NON-NEGATIVE-NUMBER-ONLY-ERROR-MESSAGE)

;(define (total-non-negative-numbers numbers) 0)

(define (total-non-negative-numbers numbers)
  (cond [(empty? numbers) 0]
        [(not (number? (first numbers))) (error NUMBER-ONLY-ERROR-MESSAGE)]
        [(negative? (first numbers)) (error NON-NEGATIVE-NUMBER-ONLY-ERROR-MESSAGE)]
        [else
         (+ (first numbers)                                
            (total-non-negative-numbers (rest numbers)))])) 
