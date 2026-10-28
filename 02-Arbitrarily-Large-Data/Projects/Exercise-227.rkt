;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-227) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =========
;; Constants

(define BLACK "black")
(define WHITE "white")

;; ===============
;; Data definition

;; FSM is one of:
;; - empty
;; - (cons Transition FSM)

(define-struct transition [current next])
;; Transition is (make-transition FSM-State FSM-State)

;; FSM-State is Color

;; Represent a FSM can transition from one state to other.

(define BW-MACHINE
  (list (make-transition BLACK WHITE)
        (make-transition WHITE BLACK)))


