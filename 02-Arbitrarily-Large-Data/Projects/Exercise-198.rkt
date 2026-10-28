;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Constants
;; =========

(define LETTERS                              ;ListOfLetter
  (explode "abcdefghijklmnopqrstuvwxyz"))

;; Data definitions
;; ================

;; Letter is one of:
;; - "a"
;; - "b"
;; - ...
;; - "z"
;; Represents a letter in alphabet.

;; Dictionary is ListOfString
;; Represent a dictionary contain list of string.

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; Respresent a list of string.

;; Functions
;; =========

;; Dictionary -> ListOfDictionary
;; produces list of dictionary contain item group starts with same letter.
(check-expect (words-by-first-letter empty) empty)
(check-expect (words-by-first-letter (list "apple" "banana" "alice"))
              (list (list "apple" "alice")
                    (list "banana")))

#;
(define (words-by-first-letter dict) empty)

(define (words-by-first-letter dict)
  (words-starts-with LETTERS dict ))

;; Letters ListOfString -> ListOfString
;; produces all item group starts with same letters.
(check-expect (words-starts-with empty empty) empty)
(check-expect (words-starts-with LETTERS empty) empty)
(check-expect (words-starts-with empty (list "apple" "banana" "alice")) empty)
(check-expect (words-starts-with LETTERS (list "apple" "banana" "alice")) (list (list "apple" "alice") (list "banana")))

;(define (words-starts-with letters los) empty)

(define (words-starts-with letters los)
  (cond [(empty? letters) empty]
        [(empty? los) empty]
        [else
         (local [(define words (start-withs-letter (first letters) los))]
           (if (> (length words) 0)
               (cons 
                words
                (words-starts-with (rest letters) los))
               (words-starts-with (rest letters) los)))]))

;; Letter ListOfString -> ListOfString
;; produces all item group with the same letter in dictionary
(check-expect (start-withs-letter "a" empty) empty)
(check-expect (start-withs-letter "a" (list "apple" "banana" "alice")) (list "apple" "alice"))

(define (start-withs-letter letter los)
  (cond [(empty? los) empty]
        [else                
         (if (starts-with? (first los) letter)
             (cons (first los) (start-withs-letter letter (rest los)))
             (start-withs-letter letter (rest los)))]))

;; Letter String -> Boolean
;; determines if string starts with given letter.
(check-expect (starts-with? "apple" "a") #true)
(check-expect (starts-with? "banana" "c") #false)

(define (starts-with? string letter)
  (string=? (substring string 0 1) letter))