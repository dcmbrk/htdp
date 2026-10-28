;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-236) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Number ListOfString -> ListOfNumber
;; adds n to each item on l.
(check-expect (plusn 1 empty) empty)
(check-expect (plusn 1 (list 1 2 3)) (list 2 3 4))
(check-expect (plusn 5 (list 1 2 3)) (list 6 7 8))

#;
(define (plusn n l) empty)

(define (plusn n l)
  (cond [(empty? l) empty]
        [else
         (cons (+ (first l) n)
               (plusn n (rest l)))]))

;; ListOfNumber -> ListOfNumber
;; adds 1 to each item on l.
(check-expect (add1* empty) empty)
(check-expect (add1* (list 1 2 3)) (list 2 3 4))

#;
(define (add1* l) empty)

(define (add1* l)
  (cond [(empty? l) empty]
        [else
         (cons (add1 (first l))
               (add1* (rest l)))]))

;; ListOfNumber -> ListOfNumber.
;; adds 5 to each item on l
(check-expect (plus5 empty) empty)
(check-expect (plus5 (list 1 2 3)) (list 6 7 8))

#;
(define (plus5 l) empty)

(define (plus5 l)
  (cond [(empty? l) empty]
        [else
         (cons (+ (first l) 5)
               (plus5 (rest l)))]))

;; ListOfNumber -> ListOfNumber
;; subtract 2 to each item on l.
(check-expect (sub2* empty) empty)
(check-expect (sub2* (list 2 4 6)) (list 0 2 4))

#;
(define (sub2* l) empty)

(define (sub2* l)
  (plusn -2 l))
