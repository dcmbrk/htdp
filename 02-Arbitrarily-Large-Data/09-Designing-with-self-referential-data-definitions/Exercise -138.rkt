;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise -138|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; Amounts is one of:
;; - '()
;; - (cons PositiveNumber Amounts)
;; Represent a amounts of money

(define A0 '())
(define A1 (cons 100 '()))
(define A2 (cons 100 (cons 200 '())))

#;
(define (fn-for-amounts amounts)
  (cond [(empty? amounts) (...)]
        [else
         (... (first amounts)                      ;PositiveNumber
              (fn-for-amounts (rest amounts)))]))  ;Amounts

;; Template rules used:
;; - one of 2 cases:
;; - atomic distinct: '()
;; - compound: (cons PositiveNumber Amounts)
;; - self-reference: (rest amounts) is Amounts


;; Functions
;; =========

;; Amounts -> Number
;; Computes the sum of the amounts
(check-expect (sum-amounts '()) 0)
(check-expect (sum-amounts (cons 100 '())) (+ 100 0))
(check-expect (sum-amounts (cons 200 (cons 100 '()))) (+ 200 100 0))

;(define (sum-amounts amounts) 0)

(define (sum-amounts amounts)
  (cond [(empty? amounts) 0]
        [else
         (+ (first amounts)                   ;PositiveNumber
            (sum-amounts (rest amounts)))]))  ;Amounts