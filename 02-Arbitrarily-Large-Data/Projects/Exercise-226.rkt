;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; =========

(define RED "red")
(define GREEN "green")
(define YELLOW "yellow")

;; ================
;; Data definitions

;; FSM is one of:
;; - empty
;; - (cons Transition FSM)

(define-struct transition [current next])
;; Transition is (make-transisiton FSM-State FSM-State)

;; FSM-State is a Color.

;; interpretation: FSM represents the transitions that a finite state machine
;; can take from one state to another in reaction to keystrokes

;; =============
;; Data examples

(define FSM-TRAFFIC
  (list (make-transition RED GREEN)
        (make-transition GREEN YELLOW)
        (make-transition YELLOW RED)))

;; =========
;; Functions

;; FSM-State FSM-State -> Boolean
;; determines the first and second states are equal.
(check-expect (state=? RED GREEN) #false)
(check-expect (state=? YELLOW YELLOW) #true)

#;
(define (state=? s1 s2) #false)

(define (state=? s1 s2)
  (string=? s1 s2))

