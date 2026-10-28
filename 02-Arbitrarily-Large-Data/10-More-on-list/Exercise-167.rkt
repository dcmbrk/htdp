;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-167) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Posns -> Natural
;; produce total x coordinate of posns.
(check-expect (sum (list (make-posn 0 0)
                         (make-posn 2 3)
                         (make-posn 4 7)))
              (+ 0 2 4))

#;
(define (sum posns) 0)

(define (sum posns)
  (cond [(empty? posns) 0]
        [else
         (+ (posn-x (first posns))
            (sum (rest posns)))]))