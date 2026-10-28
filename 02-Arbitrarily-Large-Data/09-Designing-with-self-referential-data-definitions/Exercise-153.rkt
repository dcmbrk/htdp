;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-153) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; =========

(define MTS empty-image)
(define BLOCK-SIZE 10)
(define BLOCK (square BLOCK-SIZE "outline" "black"))
(define BALLON-SIZE 2)
(define BALLON (circle BALLON-SIZE "solid" "red"))

;; Data definitions
;; ================

;; NATURAL is one of:
;; - 0
;; - (add1 NATURAL)

#;
(define (fn-for-NATURAL n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-NATURAL (sub1 n)))]))


(define-struct pos [x y])
;; Pos is (make-pos Natural Natural)
;; Represents a position in screen

#;
(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))

;; ListOfPos is one of:
;; - '()
;; - (cons Pos ListOfPos)
;; Represents a list of pos

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-pos (first lop))        ;Pos
              (fn-for-lop (rest lop)))]))     ;ListOfPos

;; Functions
;; =========

;; NATURAL Image -> Image
;; produce number of image beside each other
(check-expect (col 0 empty-image) empty-image)
(check-expect (col 0 BLOCK) empty-image)
(check-expect (col 1 BLOCK) BLOCK)
(check-expect (col 2 BLOCK) (beside BLOCK BLOCK))

;(define row MTS)

(define (col n img)
  (cond [(zero? n) MTS]
        [else
         (beside img
                 (col (sub1 n) img))]))

;; NATURAL Image -> Image
;; produce number of image above each other
(check-expect (row 0 empty-image) empty-image)
(check-expect (row 0 BLOCK) empty-image)
(check-expect (row 1 BLOCK) BLOCK)
(check-expect (row 2 BLOCK) (above BLOCK BLOCK))

;(define (col n img) empty-image)

(define (row n img)
  (cond [(zero? n) MTS]
        [else
         (above img
                (row (sub1 n) img))]))

;; NATURAL -> Image
;; render hall
(check-expect (render-hall 0 0) MTS)
(check-expect (render-hall 0 1) MTS)
(check-expect (render-hall 1 0) MTS)
(check-expect (render-hall 1 1) BLOCK)
(check-expect (render-hall 1 2) (col 2 BLOCK))
(check-expect (render-hall 2 1) (row 2 BLOCK))
(check-expect (render-hall 2 2) (row 2 (col 2 BLOCK)))

;(define (render-hall r c) MTS)

(define (render-hall r c)
  (row r (col c BLOCK)))


;; ListOfPos Image -> Image
;; render painted pos in hall
(check-expect (paint-hall empty MTS) MTS)
(check-expect (paint-hall (list (make-pos 10 10)) MTS) MTS)
(check-expect (paint-hall (list (make-pos 10 10)) (render-hall 20 10))
              (place-image BALLON 10 10 (render-hall 20 10)))
(check-expect (paint-hall (list (make-pos 10 10) (make-pos 50 50)) (render-hall 20 10))
              (place-image BALLON 50 50 (place-image BALLON 10 10 (render-hall 20 10))))

(define (paint-hall lop img)
  (cond [(empty? lop) img]
        [else
         (place-image BALLON
                      (pos-x (first lop))
                      (pos-y (first lop));Pos
                      (paint-hall (rest lop) img))]))     ;ListOfPos


;; Answer
;; ======

(define HALL (render-hall 20 10))
(define S1 (paint-hall (list (make-pos 20 30) (make-pos 90 30)) HALL))
(define S2 (paint-hall
             (list (make-pos 10 20)
                   (make-pos 20 40)
                   (make-pos 30 60)
                   (make-pos 40 80)
                   (make-pos 50 100)
                   (make-pos 60 120)
                   (make-pos 70 140)
                   (make-pos 80 160)
                   (make-pos 90 180)
                   (make-pos 100 200))
             HALL))