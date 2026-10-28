;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-152) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; =========

(define MTS empty-image)
(define BLOCK-SIZE 10)
(define BLOCK (square BLOCK-SIZE "outline" "black"))

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