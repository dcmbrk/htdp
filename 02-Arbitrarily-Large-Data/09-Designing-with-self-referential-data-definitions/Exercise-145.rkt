;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-145) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants
;; =========

(define NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE "Exception: expects non list of ctemperatures")
(define CTEMPERATURE-LIST-ERROR-MESSAGE "Exception: expects list of ctemperatures")


;; Data definitions
;; ================

;; CTemperatures is a Number greater than ABSOLUTE0

(define ABSOLUTE0 -272)

;; Temperatures is one of:
;; - '()
;; - (cons CTemperatures Temperatures)

#;
(define (fn-for-temperatures temperatures)
  (cond [(empty? temperatures) (...)]
        [(not (first temperatures)) CTEMPERATURE-LIST-ERROR-MESSAGE]
        [else
         (... (first temperatures)                             ;; CTemperatures
              (fn-for-temperatures (rest temperatures)))]))    ;; Temperatures

;; NETemperatures is one of:
;; - (cons CTemperatures '())
;; - (cons CTemperatures NETemperatures)

#;
(define (fn-for-netemperatures temperatures)
  (cond [(not (netemperatures? temperatures))
         (error NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)]
        [(empty? (rest temperatures)) (...)]
        [else
         (... (first temperatures)                            ;; CTemperatures
              (fn-for-netemperatures (rest temperatures)))])) ;; NETemperatures

;; Functions
;; =========

;; CTemperatures -> Boolean
;; Determines if given valid CTemperature
(check-expect (ctemperature? (- ABSOLUTE0 1)) #false)
(check-expect (ctemperature? ABSOLUTE0) #false)
(check-expect (ctemperature? (+ ABSOLUTE0 1)) #true)
(check-expect (ctemperature? "a") #false)
(check-expect (ctemperature? (list "a")) #false)

;(define (ctemperatures? t) #false)

(define (ctemperature? t)
  (and (number? t) (> t ABSOLUTE0)))


;; NETemperatures -> Boolean
;; Determines if given valid NETemperatures
(check-expect (netemperatures? '()) #false)
(check-expect (netemperatures? (list 1)) #true)
(check-expect (netemperatures? ABSOLUTE0) #false)
(check-expect (netemperatures? "a") #false)
(check-expect (netemperatures? (list ABSOLUTE0)) #false)
(check-expect (netemperatures? (list "a")) #false)

;(define (netemperatures? temperatures) #false)

(define (netemperatures? temperatures)
  (cond [(or (empty? temperatures)
             (not (list? temperatures))
             (not (ctemperature? (first temperatures))))
         #false]
        [(empty? (rest temperatures)) #true]
        [else
         (and (ctemperature? (first temperatures))
              (netemperatures? (rest temperatures)))]))

;; NETemperatures -> Natural
;; Determines if list is sorted

(check-expect (sorted?--net (list 1)) #true)
(check-expect (sorted?--net (list 1 2)) #true)
(check-expect (sorted?--net (list 1 3 2)) #false)
(check-error (sorted?--net '()) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (sorted?--net "a") NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (sorted?--net (list "a")) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (sorted?--net (list ABSOLUTE0)) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)

;(define (sorted?--net temperatures) #false)

(define (sorted?--net temperatures)
  (cond [(not (netemperatures? temperatures))
         (error NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)]
        [(empty? (rest temperatures)) #true]
        [else
         (and (< (first temperatures) (second temperatures))
              (sorted?--net (rest temperatures)))])) 
