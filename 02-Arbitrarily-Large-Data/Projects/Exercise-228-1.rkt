;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-228-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)

;; ===============
;; Data definition

;; A SimulationState.v1 is an FSM-State

;; =========
;; Functions

(define (simulate.v1 fsm0)
  (big-bang initial-state
    [to-draw render-state.v1]
    [on-key find-next-state.v1]))

;; SimulationState.v1 -> Image
;; renders a world state as an image

(define (render-state.v1 s) empty-image)

;; SimulationState.v1 -> SimulationState.v1
;; finds the next state from KeyEvent and current state.

(define (find-next-state.v1 cs ke) cs)