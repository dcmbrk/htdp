;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-237) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ================
;; Data definitions

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; Represents a list of number.

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; =========
;; Functions

;; (Number Number -> Boolean) ListOfNumber Number -> ListOfNumber
;; selects numbers on list that sastified function.
(check-expect (extract > empty 2) empty)
(check-expect (extract > (list 1 2 3) 2) (list 3))
(check-expect (extract < (list 1 2 3) 3) (list 1 2))

#;
(define (extract R l t) empty)

(define (extract R l t)
  (cond [(empty? l) empty]
        [else
         (if (R (first l) t)
             (cons (first l) (extract R (rest l) t))
             (extract R (rest l) t))]))

;; ListOfNumber Number -> ListOfNumber
;; selects numbers on l that are below t.
(check-expect (small empty 3) empty)
(check-expect (small (list 1 2 3) 3) (list 1 2))

#;
(define (small l t) empty)

(define (small l t)
  (extract < l t))

#;
(define (small l t)
  (cond [(empty? l) empty]
        [else
         (if (> t (first l))
             (cons (first l) (small (rest l) t))
             (small (rest l) t))]))

;; ListOfNumber Number -> ListOfNumber
;; selects numbers on l that are above t.
(check-expect (large empty 2) empty)
(check-expect (large (list 1 2 3) 2) (list 3))

#;
(define (large l t) empty)

(define (large l t)
  (extract > l t))

#;
(define (large l t)
  (cond [(empty? l) empty]
        [else
         (if (> (first l) t)
             (cons (first l) (large (rest l) t))
             (large (rest l) t))]))

;; Number Number -> Boolean
;; determines if x squared is above y.
(check-expect (squared>? 3 10) #false)
(check-expect (squared>? 4 10) #true)
(check-expect (squared>? 5 10) #true)

#;
(define (squared>? x y) #false)

(define (squared>? x y)
  (> (* x x) y))

