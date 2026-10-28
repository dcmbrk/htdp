;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-151) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants
;; =========

(define PI 3.141)

;; Data definitions
;; ================

;; NATURAL is one of:
;; - 0
;; - (add1 NATURAL)

#;
(define (fn-for-NATURAL n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-NATURAL (sub1 n)))]))

;; Functions
;; =========

;; NATURAL -> NATURAL
;; Multiply expression
(check-expect (multiply 0 0) 0)
(check-expect (multiply 1 0) 0)
(check-expect (multiply 0 1) 0)
(check-expect (multiply 1 1) 1)
(check-expect (multiply 1 2) 2)
(check-expect (multiply 2 1) 2)
(check-expect (multiply 2 2) 4)
(check-expect (multiply 2 3) 6)
(check-expect (multiply 3 3) 9)
(check-expect (multiply 3 4) 12)

;(define (multiply a b) 0)

(define (multiply a b)
  (cond [(zero? b) 0]
        [else
         (+ a
            (multiply a (sub1 b)))]))
