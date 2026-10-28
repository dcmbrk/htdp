;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-141) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constanst
;; =========

(define STRING-ONLY-ERROR-MESSAGE "Exception: expects a list of string")


;; Data definitions
;; ================

;; ListOfString is one of:
;; - '()
;; - (cons String ListOfString)
;; Respresents a list of string

(define LOS0 '())
(define LOS1 (cons "a" '()))
(define LOS2 (cons "a" (cons "b" '())))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [... (first los) (error ...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions
;; =========

;; ListOfString -> String
;; concat all string in list together
(check-expect (cat '()) "")
(check-expect (cat (cons "a" '())) "a")
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-error (cat (cons 1 '())) STRING-ONLY-ERROR-MESSAGE)

;(define (cat los) "")

(define (cat los)
  (cond [(empty? los) ""]
        [(not (string? (first los))) (error STRING-ONLY-ERROR-MESSAGE)]
        [else
         (string-append (first los)
                        (cat (rest los)))]))
