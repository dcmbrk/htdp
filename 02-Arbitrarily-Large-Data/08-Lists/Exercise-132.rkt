;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 132|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =========
;; Constanst

(define CONTACT-LIST
  (cons "Fagan"
        (cons "Findler"
              (cons "Fisler"
                    (cons "Flanagan"
                         (cons "Flatt"
                               (cons "Felleisen"
                                     (cons "Friedman"
                                           '()))))))))

;; ================
;; Data definitions

;; Names is one of:
;; - '()
;; - (cons String Names)
;; Represents a contact list of cell phone.

(cons "A"
      (cons "B"
            (cons "C" '())))

#;
(define (fn-for-names names)
  (cond [(empty? names) (...)]
        [else
         (... (first names)
              (fn-for-names (rest names)))]))

;; Template rules used:
;; - one of 2 cases:
;; - atomic distinct: '()
;; - compound: (cons String Names)
;; - self-reference: Names

;; =========
;; Functions

;; Names -> Boolean
;; Determines whether "Flatt" is on a Names
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "A" (cons "B" (cons "C" '())))) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '())))) #true)
(check-expect (contains-flatt? (cons "A" (cons "B" (cons "Flatt" '())))) #true)

;(define (contains-flatt? names) #false)

(define (contains-flatt? names)
  (cond [(empty? names) #false]
        [else
         (or (string=? (first names) "Flatt")
             (contains-flatt? (rest names)))]))


(contains-flatt? CONTACT-LIST)