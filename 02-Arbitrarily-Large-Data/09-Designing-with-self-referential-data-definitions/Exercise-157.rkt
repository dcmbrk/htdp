;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-157) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
;; =========

(define WIDTH 600)
(define HEIGHT 400)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define X-CENTER (/ WIDTH 2))
(define SHOT (triangle 3 "solid" "red"))
(define SHOT-SPEED 1)
(define SHOT-INITIAL-POSITION HEIGHT)

;; Data definitions
;; ================

;; Shot is Number
;; Represents a shot's y-coordinate

;; ListOfShot is one of:
;; - '()
;; (cons Shot ListOfShot)
;; Represents a list of shot

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)                  ;Shot
              (fn-for-los (rest los)))]))  ;ListOfShot


;; Functions
;; =========

;; ListOfShot -> ListOfShot
;; Initialize world with (main '())
;; !!!

(define (main los)
  (big-bang los
    (on-tick advance-shots)
    (to-draw render-shots)
    (on-key shots-handler)))

;; ListOfShot -> ListOfShot
;; Move every shot in the list
(check-expect (advance-shots '()) '())
(check-expect (advance-shots (list SHOT-INITIAL-POSITION))
              (list (- HEIGHT SHOT-SPEED)))
(check-expect (advance-shots (list SHOT-INITIAL-POSITION
                                   (- SHOT-INITIAL-POSITION SHOT-SPEED)))
              (list (- SHOT-INITIAL-POSITION SHOT-SPEED)
                    (- SHOT-INITIAL-POSITION (* 2 SHOT-SPEED))))

;(define (advance-shots los) los)

(define (advance-shots los)
  (cond [(empty? los) '()]
        [else
         (cons (- (first los) SHOT-SPEED)
               (advance-shots (rest los)))]))

;; ListOfShot -> Image
;; Render all shots in the background
(check-expect (render-shots '()) BACKGROUND)
(check-expect (render-shots (list SHOT-INITIAL-POSITION))
              (place-image SHOT
                           X-CENTER
                           SHOT-INITIAL-POSITION
                           BACKGROUND))

(check-expect (render-shots (list SHOT-INITIAL-POSITION
                                  (- SHOT-INITIAL-POSITION 10)))
              (place-image SHOT
                           X-CENTER
                           SHOT-INITIAL-POSITION
                           (place-image SHOT
                                        X-CENTER
                                        (- SHOT-INITIAL-POSITION 10)
                                        BACKGROUND)))

;(define (render-shots los) BACKGROUND)

(define (render-shots los)
  (cond [(empty? los) BACKGROUND]
        [else
         (place-image SHOT
                      X-CENTER
                      (first los)
                      (render-shots (rest los)))]))  ;ListOfShot

;; ListOfShot ke -> ListOfShot
;; when hit space add new shot to the list on bottom of the screen
(check-expect (shots-handler '() "a") '())
(check-expect (shots-handler '() " ")
              (list SHOT-INITIAL-POSITION))
(check-expect (shots-handler
               (list SHOT-INITIAL-POSITION)
               " ")
              (list SHOT-INITIAL-POSITION
                    SHOT-INITIAL-POSITION))

;(define (shots-handler los ke) los)

(define (shots-handler los ke)
  (if (key=? ke " ")
      (cons SHOT-INITIAL-POSITION los)
      los))