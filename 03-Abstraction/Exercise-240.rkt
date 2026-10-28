;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-240) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ================
;; Data definitions

(define-struct layer [stuff])

;; LayerString is one of:
;; - String
;; - (make-layer LayerString)
;; Represents a layer of string.

(define LS-0 "ls0")
(define LS-1 (make-layer "ls1"))
(define LS-2 (make-layer (make-layer "ls2")))

;; LayerNumber is one of:
;; - Number
;; - (make-layer LayerNumber)
;; Represents a layer of number.

(define LN-0 0)
(define LN-1 (make-layer 1))
(define LN-2 (make-layer (make-layer 2)))

;; [LayerX] is one of:
;; - X
;; - (make-layer [LayerX])
;; Represent a layer of thing.

;; LayerString is [Layer String]
;; LayerNumber is [Layer Number]