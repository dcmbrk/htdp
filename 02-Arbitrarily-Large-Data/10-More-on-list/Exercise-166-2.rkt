;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-166-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data Definitions
;; ================

(define-struct employee [name id])
;; Employee is (make-employee String Natural)
;; interp. a employee with name and id.

#;
(define (fn-for-employee e)
  (... (employee-name e)
       (employee-id e)))

(define-struct paycheck [employee amount])
;; Paycheck is (make-paycheck Employee Number)
;; interp. a paycheck contain employee and an amount

#;
(define (fn-for-paycheck p)
  (... (paycheck-employee p)
       (paycheck-amount p)))

(define-struct work [employee rate hours])
;; Work is (make String Number Number)
;; interp. a work contain employee and pay rate, number of hours 

#;
(define (fn-for-work w)
  (... (work-employee w)
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
(check-expect (wage* (list (make-work (make-employee "Robby" 1) 11.95 39)))
              (list (make-paycheck (make-employee "Robby" 1) (* 11.95 39))))
(check-expect (wage* (list (make-work (make-employee "Robby" 1) 11.95 39)
                           (make-work (make-employee "Matthew" 2) 12.95 45)))
              (list (make-paycheck (make-employee "Robby" 1) (* 11.95 39))
                    (make-paycheck (make-employee "Matthew" 2) (* 12.95 45))))

(define (wage* works)
  (cond [(empty? works) empty]
        [else
         (cons (wage (first works))
               (wage* (rest works)))]))

;; Work -> Paycheck
;; convert work to paycheck
(check-expect (wage (make-work (make-employee "Robby" 1) 11.95 39)) (make-paycheck (make-employee "Robby" 1) (* 11.95 39)))

(define (wage w)
  (make-paycheck (work-employee w)
                 (* (work-rate w) (work-hours w))))
