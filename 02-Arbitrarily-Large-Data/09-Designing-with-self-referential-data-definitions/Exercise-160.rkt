;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-160) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; SetOfNumberL is one of:
;; - '()
;; - (cons Number SetOfNumberL)
;; Represents set of number. may contain not unique number

;; SetOfNumberR is one of:
;; - '()
;; - (cons Number SetOfNubmerR)
;; Represents set of number. not contain unique number


;; Functions
;; =========

;; SetOfNumberL Number -> SetOfNumberL
;; Add number to the list
(check-expect (push--sonl '() 1) (list 1))
(check-expect (push--sonl (list 1) 1) (list 1 1))

;(define (push--sonl sonl n) sonl)

(define (push--sonl sonl n)
  (cond [(empty? sonl) (cons n '())]
        [else
         (cons n sonl)]))

;; SetOfNumberR Number -> SetOfNumberE
;; Add number to the unique list
(check-expect (push--sonr '() 1) (list 1))
(check-expect (push--sonr (list 1) 1) (list 1))
(check-expect (push--sonr (list 1) 2) (list 2 1))

;(define (push--sonr sonr n) sonr)

(define (push--sonr sonr n)
  (cond [(empty? sonr) (cons n '())]
        [else
         (if (member? n sonr)
             sonr
             (cons n sonr))]))