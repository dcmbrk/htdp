;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 188

;; Data definitions
;; ================

(define-struct email [from date message])
;; Email is (make-email String Number String)
;; Represents (make-email f d m) with sent by f, d seconds after sent, text m

#;
(define (fn-for-email e)
  (... (email-from e)
       (email-date e)
       (email-message e)))

;; Emails is one of:
;; - empty
;; - (cons Email Emails)
;; Represents a list of email.

#;
(define (fn-for-emails loe)
  (cond [(empty? loe) (...)]
        [else
         (... (fn-for-email (first loe))
              (fn-for-emails (rest loe)))]))


;; Functions
;; =========

;; Emails -> Boolean
;; Determines if list of email are sorted.
(check-expect (sorted-emails? empty) #true)
(check-expect (sorted-emails? (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
              #true)
(check-expect (sorted-emails? (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                                    (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
              #true)
(check-expect (sorted-emails? (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")
                                    (make-email "ANONYMOUS" 300 "GOODBYE WORLD")))
              #false)

(define (sorted-emails? loe)
  (cond [(or (empty? loe)
             (empty? (rest loe)))
         #true]
        [else
         (and (< (email-date (first loe)) (email-date (second loe)))
              (sorted-emails? (rest loe)))]))


;; Emails -> Emails
;; Insertion sort by date for list of email.
(check-expect (sort-emails empty) empty)
(check-expect (sort-emails (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
              (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
(check-expect (sort-emails (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                                 (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
              (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                    (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
(check-expect (sort-emails (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")
                                 (make-email "ANONYMOUS" 300 "GOODBYE WORLD")))
              (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                    (make-email "ANONYMOUS" 3600 "HELLO WORLD")))

(define (sort-emails loe)
  (cond [(empty? loe) empty]
        [else
         (insert-email (first loe)
                       (sort-emails (rest loe)))]))

;; Email Emails -> Emails
;; Insert email to sorted list of emails.
(check-expect (insert-email (make-email "ANONYMOUS" 3600 "HELLO WORLD")
                            empty)
              (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
(check-expect (insert-email (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                            (list (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
              (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                    (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
(check-expect (insert-email (make-email "ANONYMOUS" 3600 "HELLO WORLD")
                            (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")))
              (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                    (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
(check-expect (insert-email (make-email "ANONYMOUS" 500 "THANKS")
                            (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                                  (make-email "ANONYMOUS" 3600 "HELLO WORLD")))
              (list (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                    (make-email "ANONYMOUS" 500 "THANKS")
                    (make-email "ANONYMOUS" 3600 "HELLO WORLD")))


(define (insert-email e loe)
  (cond [(empty? loe) (cons e loe)]
        [else
         (if (newer-email? e (first loe))
             (cons e loe)
             (cons (first loe) (insert-email e (rest loe))))]))

;; Email Email -> Boolean
;; Determine if first email is newer.
(check-expect (newer-email? (make-email "ANONYMOUS" 300 "GOODBYE WORLD")
                            (make-email "ANONYMOUS" 3600 "HELLO WORLD"))
              #true)
(check-expect (newer-email? (make-email "ANONYMOUS" 3600 "HELLO WORLD")
                            (make-email "ANONYMOUS" 300 "GOODBYE WORLD"))
              #false)

(define (newer-email? first second)
  (< (email-date first)
     (email-date second)))
