;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-166) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data Definitions
;; ================

(define-struct paycheck [name amount])
;; Paycheck is (make String Number)
;; interp. a paycheck contain employee's name and an amount

#;
(define (fn-for-paycheck p)
  (... (paycheck-name p)
       (paycheck-amount p)))

(define-struct work [name rate hours])
;; Work is (make String Number Number)
;; interp. a work contain employee's name and pay rate, number of hours 

#;
(define (fn-for-work w)
  (... (work-name w)
       (work-rate w)
       (work-hours w)))

;; Paychecks is one of:
;; - empty
;; - (cons Paycheck Paychecks)
;; interp. a list of paycheck.

;; Works is one of:
;; - empty
;; - (cons Work Works)
;; interp. a list of work.

#;
(define (fn-for-works works)
  (cond [(empty? works) (...)]
        [else
         (... (fn-for-work (first works))
              (fn-for-works (rest works)))]))

;; Functions
;; =========

;; Works -> Paychecks
;; convert works to paychecks.
(check-expect (wage* empty) empty)
(check-expect (wage* (list (make-work "Robby" 11.95 39)))
              (list (make-paycheck "Robby" (* 11.95 39))))
(check-expect (wage* (list (make-work "Robby" 11.95 39)
                           (make-work "Matthew" 12.95 45)))
              (list (make-paycheck "Robby" (* 11.95 39))
                    (make-paycheck "Matthew" (* 12.95 45))))

#;
(define (wage* works) empty)

(define (wage* works)
  (cond [(empty? works) empty]
        [else
         (cons (wage (first works))
               (wage* (rest works)))]))

;; Work -> Paycheck
;; convert work to paycheck
(check-expect (wage (make-work "Robby" 11.95 39)) (make-paycheck "Robby" (* 11.95 39)))

#;
(define (wage work) (make-paycheck "name" 0))

(define (wage w)
  (make-paycheck (work-name w)
                 (* (work-rate w) (work-hours w))))
