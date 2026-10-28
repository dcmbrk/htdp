;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-155) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

(define-struct layer [color doll])
;; Doll is (make-layer String RD)
;; Represents a layer of a doll

;; RD (short for Russian doll) is one of:
;; - String
;; - (make-layer String RD)
;; Represents a russian doll

#;
(define (fn-for-layer l)
  (... (layer-color l)                        ;String
       (fn-for-rd (layer-doll l))))           ;RD
#;
(define (fn-for-rd rd)
  (cond [(string? rd) (...)]                 ;String
        [(layer? rd)
         (... (layer-color rd)
              (fn-for-rd (layer-doll rd)))]))

;; Functions
;; =========

;; RD -> String
;; produce mostinner color of doll
(check-expect (inner "red") "red")
(check-expect (inner (make-layer "yellow" "green")) "green")
(check-expect (inner (make-layer "yellow" (make-layer "green" "red"))) "red")

;(define (inner rd) "")

(define (inner rd)
  (cond [(string? rd) rd]                 ;String
        [(layer? rd)
         (inner (layer-doll rd))]))
