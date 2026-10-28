;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 190

;; Data definitions
;; ================

;; (listof 1String) is one of:
;; - empty
;; - (cons 1String (listof 1String))
;; Represents a list of characters.

#;
(define (fn-for-lo1s lo1s)
  (cond [(empty? lo1s) (...)]
        [else
         (... (first lo1s)
              (fn-for-lo1s (rest lo1s)))]))

;; (listof (listof 1String)) is one of:
;; - empty
;; - (cons (listof 1String) (listof (listof 1String)))
;; Represents a list contain a list of characters.

#;
(define (fn-for-lolo1s lolo1s)
  (cond [(empty? lolo1s) (...)]
        [else
         (... (fn-for-lo1s (first lolo1s))       ;(listof 1String)
              (fn-for-lolo1s (rest lolo1s)))]))  ;(listof (listof 1String))

;; Functions
;; =========

;; (listof X) -> (listof X)
;; Remove last item in list.
(check-expect (blast empty) empty)
(check-expect (blast (list 1)) empty)
(check-expect (blast (list 1 2)) (list 1))

(define (blast lox)
  (cond [(or (empty? lox)
             (empty? (rest lox)))
         empty]
        [else
         (cons (first lox)
               (blast (rest lox)))]))


;; (listof 1String) ->  (listof (listof 1String))
;; Produces list of prefixes from given list.
(check-expect (prefixes empty) (list empty))
(check-expect (prefixes (list "a"))
              (list empty
                    (list "a")))
(check-expect (prefixes (list "a" "b" "c"))
              (list empty
                    (list "a")
                    (list "a" "b")
                    (list "a" "b" "c")))

(define (prefixes lo1s)
  (local [(define (prefixes lo1s)
            (cond [(empty? lo1s) (list empty)]                 ;(listof (listof 1String))
                  [else
                   (append (list lo1s)                         ;(listof (listof 1String))
                           (prefixes (blast lo1s)))]))]
    (reverse (prefixes lo1s))))                                ;(listof (listof 1String))


;; (listof 1String) ->  (listof (listof 1String))
;; Produces list of prefixes from given list.
(check-expect (suffixes empty) (list empty))
(check-expect (suffixes (list "a"))
              (list (list "a")
                    empty))
(check-expect (suffixes (list "a" "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "c")
                    (list "c")
                    empty))

(define (suffixes lo1s)
  (cond [(empty? lo1s) (list empty)]
        [else
         (append (list lo1s)
                 (suffixes (rest lo1s)))]))