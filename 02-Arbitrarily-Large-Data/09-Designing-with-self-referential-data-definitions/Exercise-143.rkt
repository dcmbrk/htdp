;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-143) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;; produce size of list
(check-expect (size--net (list 1)) 1)
(check-expect (size--net (list 1 2)) 2)
(check-expect (size--net (list 1 2 3)) 3)
(check-error (size--net '()) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (size--net "a") NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (size--net (list "a")) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (size--net (list ABSOLUTE0)) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)

;(define (size--net netemperatures) 0)

(define (size--net temperatures)
  (cond [(not (netemperatures? temperatures))
         (error NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)]
        [(empty? (rest temperatures)) 1]
        [else
         (+ 1
            (size--net (rest temperatures)))]))

;; NETemperatures -> Number
;; produce total of temperatures in list
(check-expect (total--net (list 1)) (+ 0 1))
(check-expect (total--net (list 1 2)) (+ 0 1 2))
(check-error (total--net '()) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (total--net "a") NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (total--net (list "a")) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)
(check-error (total--net (list ABSOLUTE0)) NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)

;(define (total--net temperatures) 0)

(define (total--net temperatures)
  (cond [(not (netemperatures? temperatures))
         (error NON-EMPTY-CTEMPERATURE-LIST-ERROR-MESSAGE)]
        [(empty? (rest temperatures)) (first temperatures)]
        [else
         (+ (first temperatures)
            (total--net (rest temperatures)))]))

;; NETemperatures -> Number
;; produce average of list
(check-expect (average--net (list 1)) (/ (+ 1 0) 1))
(check-expect (average--net (list 1 2)) (/ (+ 1 2 0) 2))

;(define (average--net temperatures) 0)

(define (average--net temperatures)
  ( /(total--net temperatures)
     (size--net temperatures))) 
