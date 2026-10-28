;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-196) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constants
;; =========

(define LOCATION "words.txt")
(define AS-LIST (read-lines LOCATION))    ;ListOfString
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz")) ;ListOf1String

;; Data definitions
;; ================

;; A Dictionary is a ListOfString
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)                    ;String   
              (fn-for-los (rest los)))]))    ;ListOfString


;; A Letter is one of:
;; - "a"
;; - "b"
;; - ...
;; - "z"
;; or is a members? of LETTERS


(define-struct letter-count [letter count])
;; LetterCount is (make-letter-count 1String Natural)

;; Functions
;; =========

;; Letter Dictionary -> Natural
;; counts how many words in given Dictionary start with given Letter.
(check-expect (starts-with# "e" AS-LIST) 3307)
(check-expect (starts-with# "z" AS-LIST) 151)

#;
(define (starts-with# letter dict) 0)

(define (starts-with# letter dict)
  (cond [(empty? dict) 0]
        [else
         (if (start-with letter (first dict))
             (+ 1 (starts-with# letter (rest dict)))
             (starts-with# letter (rest dict)))]))

;; Letter String -> Boolean
;; determines if String starts with given Letter.
(check-expect (start-with "a" "a") #true)
(check-expect (start-with "a" "ab") #true)
(check-expect (start-with "a" "b") #false)
(check-expect (start-with "b" "ab") #false)

(define (start-with letter string)
  (string=? letter
            (substring string 0 1)))


;; Dictionary -> ListOfLetterCount
;; produce ListOfLetterCount from given Dictionary
(check-expect (count-by-letter# empty) empty)
(check-expect (count-by-letter# (list "apple"))
              (list (make-letter-count "a" 1)))
(check-expect (count-by-letter# (list "apple" "aisle"))
              (list (make-letter-count "a" 2)))
(check-expect (count-by-letter# (list "apple" "banana" "aisle"))
              (list (make-letter-count "a" 2)
                    (make-letter-count "b" 1)))

(define (count-by-letter# dict)
  (count-by-letter LETTERS dict))


;; Letters Dictionary -> ListOfLetterCount
;; produce ListOfLetterCount from given Dictionary and Letters
(check-expect (count-by-letter LETTERS empty) empty)
(check-expect (count-by-letter LETTERS (list "apple"))
              (list (make-letter-count "a" 1)))
(check-expect (count-by-letter LETTERS (list "apple" "aisle"))
              (list (make-letter-count "a" 2)))
(check-expect (count-by-letter LETTERS (list "apple" "banana" "aisle"))
              (list (make-letter-count "a" 2)
                    (make-letter-count "b" 1)))

(define (count-by-letter letters dict)
  (cond [(empty? letters) empty]
        [else
         (local [(define count (starts-with# (first letters) dict))]
           (if (> count 0)
               (cons (make-letter-count
                      (first letters)
                      count)
                     (count-by-letter (rest letters) dict))
               (count-by-letter (rest letters) dict)))]))

;; Application
;; ===========
