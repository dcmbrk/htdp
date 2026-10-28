;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-147) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constanst
;; =========

(define NON-EMPTY-BOOLEANS-ERROR-MESSAGE "[Exception]: expects list none empty booleans")


;; Data definitions
;; ================

;; NEBooleans is one of:
;; - (cons Boolean '())
;; - (cons Boolean NEBooleans)

#;
(define (fn-for-nebooleans nebooleans)
  (cond [(not (nebooleans? nebooleans)) (error NON-EMPTY-BOOLEANS-ERROR-MESSAGE)]
        [(empty? (rest nebooleans)) (... (first nebooleans))]
        [else
         (... (first nebooleans)
              (fn-for-nebooleans (rest nebooleans)))]))


;; Functions
;; =========

;; Any -> Boolean
;; Determines if any is NEBooleans
(check-expect (nebooleans? 1) #false)
(check-expect (nebooleans? '()) #false)
(check-expect (nebooleans? (list 1)) #false)
(check-expect (nebooleans? (list #true)) #true)
(check-expect (nebooleans? (list #true #false)) #true)

(define (nebooleans? any)
  (and (list? any)
       (not (empty? any))
       (not (empty? (first any)))
       (boolean? (first any))))

;; NEBooleans -> Boolean
;; produce if given list has all true
(check-expect (all-true--neb (list #true)) #true)
(check-expect (all-true--neb (list #true #true)) #true)
(check-expect (all-true--neb (list #false)) #false)
(check-expect (all-true--neb (list #true #false)) #false)
(check-error (all-true--neb "a") NON-EMPTY-BOOLEANS-ERROR-MESSAGE)
(check-error (all-true--neb '()) NON-EMPTY-BOOLEANS-ERROR-MESSAGE)
(check-error (all-true--neb (list "a")) NON-EMPTY-BOOLEANS-ERROR-MESSAGE)
(check-error (all-true--neb (list #true "a")) NON-EMPTY-BOOLEANS-ERROR-MESSAGE)

;(define (all-true--neb nebooleans) #false)

(define (all-true--neb nebooleans)
  (cond [(not (nebooleans? nebooleans)) (error NON-EMPTY-BOOLEANS-ERROR-MESSAGE)]
        [(empty? (rest nebooleans)) (first nebooleans)]
        [else
         (and (first nebooleans)
              (all-true--neb (rest nebooleans)))]))

;; NEBooleans -> Boolean
;; produce if given list has at least one true
(check-expect (one-true--neb (list #true)) #true)
(check-expect (one-true--neb (list #true #true)) #true)
(check-expect (one-true--neb (list #false)) #false)
(check-expect (one-true--neb (list #true #false)) #true)
(check-error (one-true--neb "a") NON-EMPTY-BOOLEANS-ERROR-MESSAGE)
(check-error (one-true--neb '()) NON-EMPTY-BOOLEANS-ERROR-MESSAGE)
(check-error (one-true--neb (list "a")) NON-EMPTY-BOOLEANS-ERROR-MESSAGE)
(check-error (one-true--neb (list #false "a")) NON-EMPTY-BOOLEANS-ERROR-MESSAGE)

(define (one-true--neb nebooleans)
  (cond [(not (nebooleans? nebooleans)) (error NON-EMPTY-BOOLEANS-ERROR-MESSAGE)]
        [(empty? (rest nebooleans)) (first nebooleans)]
        [else
         (or (first nebooleans)
              (one-true--neb (rest nebooleans)))]))
