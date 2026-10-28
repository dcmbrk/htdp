;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-142) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; =========

(define I0 empty-image)
(define I1 (square 10 "solid" "black"))
(define I2 (rectangle 20 10 "solid" "black"))
(define I3 (square 30 "solid" "black"))
(define I4 (rectangle 40 50 "solid" "black"))

(define ONLY-IMAGE-ERROR-MESSAGE "Exception: expects a list of images")
(define NUMBER-ERROR-MESSAGE "Exception: expect a number")
(define POSITIVE-NUMBER-ERROR-MESSAGE "Exception: expect a positive number")

;; Data definitions
;; ================

;; ImageOrFalse is one of:
;; - Image
;; - #false


;; Images is one of:
;; - '()
;; - (cons Image Images)

(define LOI0 '())
(define LOI1 (cons I1 '()))
(define LOI2 (cons I2 (cons I1 '())))
(define LOI3 (cons I3 (cons I2 (cons I1 '()))))
(define LOI4 (cons I4 (cons I3 (cons I2 (cons I1 '())))))

#;
(define (fn-for-images images)
  (cond [(empty? images) (...)]
        [(... (first images)) (...)]
        [else
         (... (first images)
              (fn-for-images (rest images)))]))

;; Functions

;; Images PositiveNumber -> ImageOrFalse
;; produces first image is not square of number, given false if not found
(check-expect (ill-sized? LOI0 20) #false)
(check-expect (ill-sized? LOI1 10) #false)
(check-expect (ill-sized? LOI2 10) I2)
(check-expect (ill-sized? LOI3 20) I3)
(check-expect (ill-sized? LOI4 10) I4)
(check-error (ill-sized? "a" "b") ONLY-IMAGE-ERROR-MESSAGE)
(check-error (ill-sized? (cons "a" '()) 10) ONLY-IMAGE-ERROR-MESSAGE)
(check-error (ill-sized? LOI4 "a") NUMBER-ERROR-MESSAGE)
(check-error (ill-sized? LOI4 -1) POSITIVE-NUMBER-ERROR-MESSAGE)

;(define (ill-sized? images size) #false)

(define (ill-sized? images size)
  (cond [(not (images? images)) (error ONLY-IMAGE-ERROR-MESSAGE)]
        [(not (number? size)) (error NUMBER-ERROR-MESSAGE)]
        [(not (positive? size)) (error POSITIVE-NUMBER-ERROR-MESSAGE)]
        [(empty? images) #false]
        [else
         (if (and (= (image-width (first images)) size)
                  (= (image-height (first images)) size))
              (ill-sized? (rest images) size)
              (first images))]))


;; ListOfImage -> Boolean
;; Determines if is list of images
(check-expect (images? LOI0) #true)
(check-expect (images? LOI1) #true)
(check-expect (images? LOI2) #true)
(check-expect (images? "a") #false)
(check-expect (images? (cons "a" '())) #false)

;(define (images? images) #false)

(define (images? images)
  (cond [(not (list? images)) #false]
        [(empty? images) #true]
        [else
         (and (image? (first images))
              (images? (rest images)))]))
