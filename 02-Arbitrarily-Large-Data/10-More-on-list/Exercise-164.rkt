;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-164) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constant
;; ========

(define RATE 0.9242)

;; Data definitions
;; ================

;; Numbers is one of:
;; - empty
;; - (cons Number Numbers)
;; interp. a list of number

#;
(define (fn-for-numbers numbers)
  (cond [(empty? numbers) (...)]
        [else
         (... (first numbers)
              (fn-for-numbers (rest numbers)))]))

;; Functions
;; =========

;; Numbers -> Numbers
;; Convert USD to Euro
(check-expect (convert-euro empty) empty)
(check-expect (convert-euro (list 32 100)) (list (* 32 RATE) (* 100 RATE)))

;(define (convert-euro numbers) empty)

(define (convert-euro numbers)
  (cond [(empty? numbers) empty]
        [else
         (cons (* (first numbers) RATE)
               (convert-euro (rest numbers)))]))

;; Numbers Number -> Numbers
;; Convert USD to Euro with given rate
(check-expect (convert-euro* empty RATE) (convert-euro empty))
(check-expect (convert-euro* (list 32 100) RATE) (convert-euro (list 32 100)))

(define (convert-euro* numbers rate)
  (cond [(empty? numbers) empty]
        [else
         (cons (* (first numbers) rate)
               (convert-euro* (rest numbers) rate))]))