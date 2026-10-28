;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-209) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; Word is one of:
;; - empty
;; - (cons 1String Word)
;; Represents a list of 1String (letters)

#;
(define (fn-for-word w)
  (cond [(empty? w) (...)]
        [else
         (... (first w)                      ;1String
              (fn-for-word (rest w)))]))     ;Word

;; Functions
;; =========

;; String -> Word
;; converts s to chosen word representation.
(check-expect (string->word "cat") (list "c" "a" "t"))
(check-expect (string->word "") empty)

#;
(define (string->word s) empty)

(define (string->word s)
  (cond [(string=? s "") empty]
        [else
         (cons (substring s 0 1)
               (string->word (substring s 1 (string-length s))))]))

;; Word -> String
;; converts w to a string
(check-expect (word->string empty) "")
(check-expect (word->string (list "c" "a" "t")) "cat")

#;
(define (word->string w) "")

(define (word->string w)
  (cond [(empty? w) ""]
        [else
         (string-append (first w)                  
                        (word->string (rest w)))]))
