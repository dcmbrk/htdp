;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-169) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constant
;; ========

(define UPPER-X 100)
(define LOWER-X   0)
(define UPPER-Y 200)
(define LOWER-Y   0)

;; Data definitions
;; ================

;; Posn is (make-posn Natural Natural)
;; interp. a position with x and y.

(define (fn-for-posn p)
  (... (posn-x p)
       (posn-y p)))

;; Posns is one of:
;; - empty
;; - (cons Posn Posns)
;; interp. a list of posn.

#;
(define (fn-for-posns posns)
  (cond [(empty? posns) (...)]
        [else
         (... (fn-for-posn (first posns))
              (fn-for-posns (rest posns)))]))

;; Functions
;; =========

;; Posns -> Posns
;; filter posns with x and y in range
(check-expect (legal empty) empty)
(check-expect (legal (list (make-posn 50 100)
                           (make-posn 150 50)
                           (make-posn 10 -5)))
              (list (make-posn 50 100)))

;(define (legal posns) empty)

(define (legal posns)
  (cond [(empty? posns) empty]
        [else
         (if (legal? (first posns))
             (cons (first posns) (legal (rest posns)))
             (legal (rest posns)))]))

;; Posn -> Boolean
;; Determines if posn in range
(check-expect (legal? (make-posn 50 100)) #true)
(check-expect (legal? (make-posn 150 50)) #false)

(define (legal? p)
  (and (> (posn-x p) LOWER-X)
       (< (posn-x p) UPPER-X)
       (> (posn-y p) LOWER-Y)
       (< (posn-y p) UPPER-Y)))
