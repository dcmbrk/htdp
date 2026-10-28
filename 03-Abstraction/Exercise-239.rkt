;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-239) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; A [List X Y] is (list X Y)
;; Represent a list contains 2 items: type X and type Y.

;; A [List Number Number] is (list Number Number)
;; Represent a list contains 2 items: Number and Number.

(define LNN (list 4 1))

;; A [List Number 1String] is (list Number 1String)
;; Represent a list contains 2 items: Number and 1String.

(define LN1S (list 4 "I"))

;; A [List String Boolean] is (list String Boolean)
;; Represent a list contains 2 items: String and Boolean.

(define LSB (list "T" #true))
