;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-150) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; NATURAL -> Number
;; Add PI to given NATURAL
(check-expect (ADD-PI 0) PI)
(check-expect (ADD-PI 1) (+ 1 PI))
(check-expect (ADD-PI 2) (+ 1 1 PI))

;(define (ADD-PI n) 0)

(define (ADD-PI n)
  (cond [(zero? n) PI]
        [else
         (add1 (ADD-PI (sub1 n)))]))
