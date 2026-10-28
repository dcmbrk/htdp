;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-170) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

(define-struct phone [area switch four])
;; Phone is (make-phone Three Three Four)
;; interp. a phone number

#;
(define (fn-for-phone p)
  (... (phone-area p)
       (phone-switch p)
       (phone-four p)))

;; Three is Number[100, 999]
;; Four is  Number[1000, 9999]

;; Phones is one of:
;; - empty
;; - (cons Phone Phones)
;; interp. a list of phone number

#;
(define (fn-for-phones phones)
  (cond [(empty? phones) (...)]
        [else
         (... (fn-for-phone (first phones))
              (fn-for-phones (rest phones)))]))

;; Functions
;; =========

;; Phones -> Phones
;; replace all area code 713 with 281
(check-expect (replace empty) empty)
(check-expect (replace (list (make-phone 555 111 5555)
                             (make-phone 713 222 7777)))
              (list (make-phone 555 111 5555)
                    (make-phone 281 222 7777)))

;(define (replace phones) empty)

(define (replace phones)
  (local [(define (fn-for-phone p)
            (if (= (phone-area p) 713)
                (make-phone 281
                            (phone-switch p)
                            (phone-four p))
                p))
          
          (define (fn-for-phones phones)
            (cond [(empty? phones) empty]
                  [else
                   (cons (fn-for-phone (first phones))
                         (fn-for-phones (rest phones)))]))]
    (fn-for-phones phones)))




