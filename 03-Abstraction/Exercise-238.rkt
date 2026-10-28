;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; NELON is one of:
;; - (cons Number empty)
;; - (cons Number ListOfNumber)
;; Represents a non empty list of number.

#;
(define (fn-for-nelon nelon)
  (cond [(empty? (rest nelon)) (...)]
        [else
         (... (first nelon)
              (fn-for-nelon (rest nelon)))]))

;; Functions
;; =========

;; NELON -> Number
;; produces the smallest number on l.
(check-expect (inf (list 1)) 1)
(check-expect (inf (list 3 2 1 4)) 1)

#;
(define (inf l) 0)

(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (< (first l) (inf (rest l)))
             (first l)
             (inf (rest l)))]))

;; NELON -> Number
;; produces the largest number on l.
(check-expect (sup (list 1)) 1)
(check-expect (sup (list 3 2 4 1)) 4)

#;
(define (sup l) 0)

(define (sup l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (> (first l) (sup (rest l)))
             (first l)
             (sup (rest l)))]))