;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ================
;; Data definitions

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; represents a list of number.

(define LON0 empty)
(define LON1 (cons 1 empty))
(define LON2 (cons 2 (cons 1 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; =========
;; Functions

;; ListOfNumber -> Number
;; computes the sum of the numbers on l.
(check-expect (sum empty) 0)
(check-expect (sum (list 1)) (+ 1 0))
(check-expect (sum (list 1 2)) (+ 1 2 0))

#;
(define (sum l) 0)

#;
(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))

(define (sum l)
  (FOLD l + 0))

;; ListOfNumber -> Number
;; computes the product of the numbers on l.
(check-expect (product empty) 1)
(check-expect (product (list 1)) (* 1 1))
(check-expect (product (list 2 1)) (* 2 1 1))

#;
(define (product l) 1)

#;
(define (product lon)
  (cond [(empty? lon) 1]
        [else
         (* (first lon)
            (product (rest lon)))]))

(define (product l)
  (FOLD l * 1))

;; [ListOfX] [X X -> X] X -> X
;; Fold function.
(check-expect (FOLD (list 2 1) * 1) (* 2 1 1))
(check-expect (FOLD (list 2 1) + 0) (+ 2 1 0))

#;
(define (FOLD lox f b) b)

(define (FOLD lon f b)
  (cond [(empty? lon) b]
        [else
         (f (first lon)
            (FOLD (rest lon) f b))]))