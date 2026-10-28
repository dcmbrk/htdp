;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-168) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constant
;; ========

(define DISTANCE 1)

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
;; translate posns to distance a straight line.
(check-expect (translate empty) empty)
(check-expect (translate (list (make-posn 0 0)
                               (make-posn 3 4)
                               (make-posn 4 7)))
              (list (make-posn 0 (+ DISTANCE 0))
                    (make-posn 3 (+ DISTANCE 4))
                    (make-posn 4 (+ DISTANCE 7))))

(define (translate posns)
  (cond [(empty? posns) empty]
        [else
         (cons (translation (first posns))
               (translate (rest posns)))]))

;; Posn -> Posn
;; translation distance a straight line.
(check-expect (translation (make-posn 0 0))
              (make-posn 0 (+ DISTANCE 0)))

;(define (translation posn) posn)

(define (translation p)
  (make-posn (posn-x p)
             (+ DISTANCE (posn-y p))))