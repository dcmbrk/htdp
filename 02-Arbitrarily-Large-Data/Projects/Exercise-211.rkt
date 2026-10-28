;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-211) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants
;; =========

(define DICTIONARY
  (list "cat" "apple"))

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

;; (listof Word) is one of:
;; empty
;; (cons Word (listof Word))
;; Represents a list of word.

#;
(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-word (first low))   ;Word
              (fn-for-low (rest low)))])) ;(listof Word)

;; Data examples
;; =============

(define WORD-0 empty)
(define WORD-1 (list "c" "a" "t"))
(define WORD-2 (list "a" "p" "p" "l" "e"))

(define LOW-0 empty)
(define LOW-1 (list WORD-1))
(define LOW-2 (list WORD-1 WORD-2))

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

;; (listof Word) -> (listof String)
;; convert list of word to list of string.
(check-expect (words->strings empty) empty)
(check-expect (words->strings (list (list "c" "a" "t")
                                    (list "a" "p" "p" "l" "e")))
              (list "cat" "apple"))

#;
(define (words->strings words) empty)

(define (words->strings low)
  (cond [(empty? low) empty]
        [else
         (cons (word->string (first low))      ;Word
               (words->strings (rest low)))])) ;(listof Word)

;; (listof String) -> (listof String)
;; picks out all string that in dictionary
(check-expect (in-dictionary empty) empty)
(check-expect (in-dictionary (list "cat" "apple")) (list "cat" "apple"))
(check-expect (in-dictionary (list "qat" "apple")) (list "apple"))

#;
(define (in-dictionary los) empty)

(define (in-dictionary los)
  (cond [(empty? los) empty]
        [else
         (if (in-dictionary? (first los))
             (cons (first los) (in-dictionary (rest los)))
             (in-dictionary (rest los)))]))

;; String -> Boolean
;; determines if string in dictionary.
(check-expect (in-dictionary? "apple") #true)
(check-expect (in-dictionary? "qat") #false)

#;
(define (in-dictionary? s) #false)

(define (in-dictionary? s)
  (member? s DICTIONARY))