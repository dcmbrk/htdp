;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-228) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; =========
;; Constants

(define RED "red")
(define GREEN "green")
(define YELLOW "yellow")
(define BLACK "black")
(define WHITE "white")

(define SIZE 100)

;; ================
;; Data definitions

(define-struct fs [fsm current])
;; SimulationState is (make-fs FSM FSM-State)

;; FSM is one of:
;; - empty
;; - (cons Transition FSM)

(define-struct transition [current next])
;; Transition is (make-transisiton FSM-State FSM-State)

;; FSM-State is a Color.

;; interpretation: FSM represents the transitions that a finite state machine
;; can take from one state to another in reaction to keystrokes

#;
(define (fn-for-fs fs)
  (... (fn-for-fsm (fs-fsm fs))          ;FSM
       (fs-current fs)))                 ;FSM-State
#;
(define (fn-for-fsm fsm)
  (cond [(empty? fsm) (...)]
        [else
         (... (fn-for-transition (first fsm)) ;Transition
              (fn-for-fsm (rest fsm)))]))     ;FSM
#;
(define (fn-for-transition t)
  (... (transition-current t)  ;FSM-State
       (transition-next t)))   ;FSM-State

;; KFSM is one of:
;; - empty
;; - (cons KTransition KFSM)

(define-struct ktransition [current key next])
;; KTransition is (make-ktransition FSM-State KeyEvent FSM-State)

;; =============
;; Data examples

(define FSM-TRAFFIC
  (list (make-transition RED GREEN)
        (make-transition GREEN YELLOW)
        (make-transition YELLOW RED)))

(define BW-MACHINE
  (list (make-transition BLACK WHITE)
        (make-transition WHITE BLACK)))

(define K-MACHINE
  (list (make-ktransition WHITE "a" YELLOW)
        (make-ktransition YELLOW "b" YELLOW)
        (make-ktransition YELLOW "c" YELLOW)
        (make-ktransition YELLOW "d" GREEN)))

;; =========
;; Functions

;; FSM FSM-State -> SimulationState
;; match the key pressed with given FSM.

(define (simulate fsm s0)
  (big-bang (make-fs fsm s0)
      [to-draw state-as-colored-square]
      [on-key find-next-state]))

;; SimulationState -> Image
;; renders a world state as an square image.
(check-expect (state-as-colored-square
               (make-fs FSM-TRAFFIC RED))
              (square SIZE "solid" RED))

#;
(define (state-as-colored-square s) empty-image)

(define (state-as-colored-square fsm)
  (square SIZE "solid" (fs-current fsm)))

;; SimulationState KeyEvent -> SimulationState
;; finds the next state from fsm and ke.
(check-expect
 (find-next-state (make-fs FSM-TRAFFIC RED) "n")
 (make-fs FSM-TRAFFIC GREEN))
(check-expect
 (find-next-state (make-fs FSM-TRAFFIC RED) "a")
 (make-fs FSM-TRAFFIC GREEN))
(check-expect
 (find-next-state (make-fs FSM-TRAFFIC GREEN) "q")
 (make-fs FSM-TRAFFIC YELLOW))

#;
(define (find-next-state fs ke) fs)

(define (find-next-state fs ke)          
  (make-fs                       ;SimulationState
   (fs-fsm fs)                   ;FSM
   (find                         ;FSM-State
    (fs-fsm fs)                  ;FSM
    (fs-current fs))))           ;FSM-State  

;; FSM FSM-State -> FSM-State
;; finds the state representing current in trasitions
(check-expect (find FSM-TRAFFIC RED) GREEN)
(check-expect (find FSM-TRAFFIC GREEN) YELLOW)
(check-error (find FSM-TRAFFIC "black") "not found: black")

#;
(define (find transitions current) current)

(define (find transitions current)
  (cond [(empty? transitions) (error (string-append "not found: " current))]
        [else
         (if (state=? current                                 
                      (transition-current (first transitions)))
             (transition-next (first transitions))
             (find (rest transitions) current))]))
#;
(define (fn-for-transition t)
  (... (transition-current t)  ;FSM-State
       (transition-next t)))   ;FSM-State

;; FSM-State FSM-State -> Boolean
;; determines the first and second states are equal.
(check-expect (state=? RED GREEN) #false)
(check-expect (state=? YELLOW YELLOW) #true)

#;
(define (state=? s1 s2) #false)

(define (state=? s1 s2)
  (string=? s1 s2))

;; ===========
;; Application

;(simulate BW-MACHINE BLACK)
(simulate FSM-TRAFFIC GREEN)

