;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 189

;; Data definitions
;; ================

;; Numbers is one of:
;; - empty
;; - (cons Number Numbers)
;; Represents a list of numbers.

#;
(define (fn-for-numbers numbers)
  (cond [(empty? numbers) (...)]
        [else
         (... (first numbers)
              (fn-for-numbers (rest numbers)))]))

;; Functions
;; =========

;; Number Numbers -> Boolean
;; seach a number in sorted list
(check-expect (search-sorted 4 empty) #false)
(check-expect (search-sorted 1 (list 4 3 2 1)) #true)
(check-expect (search-sorted 2 (list 4 3 2 1)) #true)
(check-expect (search-sorted 4 (list 4 3 2 1)) #true)
(check-expect (search-sorted 5 (list 4 3 2 1)) #false)

(define (search-sorted number numbers)
  (cond [(or (empty? numbers)
             (> number (first numbers)))
         #false]
        [(= number (first numbers)) #true]
        [else
         (search-sorted number (rest numbers))]))
