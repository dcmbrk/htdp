;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-230) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ====================
;; Constants

(define WHITE "white")
(define YELLOW "yellow")
(define GREEN "green")
(define RED "red")

(define SIZE 100)

;; ====================
;; Data Definitions

(define-struct fsm [initial transitions final])
;; An FSM.v2 is a structure:
;;   (make-fsm FSM-State LOT FSM-State)
;; interpretation: initial represents the current state of the simulation,
;; transitions is the list of allowed transitions, and final is the target state.

;; A LOT (List of Transitions) is one of:
;; - '()
;; - (cons Transition.v3 LOT)

(define-struct transition [current key next])
;; A Transition.v3 is a structure:
;;   (make-transition FSM-State KeyEvent FSM-State)

;; FSM-State is a Color.

;; ====================
;; Data Examples

;; FSM đại diện cho biểu thức chính quy: a (b|c)* d
(define FSM-109
  (make-fsm WHITE
            (list (make-transition WHITE "a" YELLOW)
                  (make-transition YELLOW "b" YELLOW)
                  (make-transition YELLOW "c" YELLOW)
                  (make-transition YELLOW "d" GREEN))
            GREEN))

;; ====================
;; Functions

;; FSM.v2 -> FSM.v2
;; Runs the given FSM.v2 simulation and stops when it reaches the final state.
(define (fsm-simulate an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final-state?]))

;; FSM.v2 -> Image
;; Renders the current state (initial) of the FSM as a colored square.
(check-expect (state-as-colored-square FSM-109)
              (square SIZE "solid" WHITE))

(define (state-as-colored-square an-fsm)
  (square SIZE "solid" (fsm-initial an-fsm)))

;; FSM.v2 KeyEvent -> FSM.v2
;; Finds the next state based on the current state and key pressed,
;; producing an updated FSM.v2 structure.
(check-expect (find-next-state FSM-109 "a")
              (make-fsm YELLOW (fsm-transitions FSM-109) GREEN))

(define (find-next-state an-fsm ke)
  (make-fsm (find (fsm-transitions an-fsm) (fsm-initial an-fsm) ke)
            (fsm-transitions an-fsm)
            (fsm-final an-fsm)))

;; LOT FSM-State KeyEvent -> FSM-State
;; Finds the next state corresponding to the current state and key in transitions.
(check-expect (find (fsm-transitions FSM-109) WHITE "a") YELLOW)
(check-expect (find (fsm-transitions FSM-109) YELLOW "b") YELLOW)
(check-expect (find (fsm-transitions FSM-109) YELLOW "c") YELLOW)
(check-expect (find (fsm-transitions FSM-109) YELLOW "d") GREEN)
(check-error (find (fsm-transitions FSM-109) WHITE "b") "not found: white with key b")

(define (find transitions current ke)
  (cond
    [(empty? transitions)
     (error (string-append "not found: " current " with key " ke))]
    [else
     (if (and (state=? current (transition-current (first transitions)))
              (key=? ke (transition-key (first transitions))))
         (transition-next (first transitions))
         (find (rest transitions) current ke))]))

;; FSM.v2 -> Boolean
;; Determines if the current state (initial) has reached the final state.
(check-expect (final-state? FSM-109) #false)
(check-expect (final-state? (make-fsm GREEN (fsm-transitions FSM-109) GREEN)) #true)

(define (final-state? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

;; FSM-State FSM-State -> Boolean
;; Determines if two states are equal.
(check-expect (state=? RED GREEN) #false)
(check-expect (state=? YELLOW YELLOW) #true)

(define (state=? s1 s2)
  (string=? s1 s2))

;; ====================
;; Application

(fsm-simulate FSM-109)