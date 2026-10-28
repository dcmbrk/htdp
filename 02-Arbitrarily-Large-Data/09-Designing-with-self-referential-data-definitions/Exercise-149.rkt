;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-149) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Functions
;; =========

;; Any Natural -> ListOfAny
;; produce list of any
(check-expect (copier 1 0) empty)
(check-expect (copier 1 1) (list 1))
(check-expect (copier "a" 2) (list "a" "a"))

;(define (copier a n) '())

(define (copier a n)
  (if (zero? n) '()
      (append (list a) (copier a (sub1 n)))))