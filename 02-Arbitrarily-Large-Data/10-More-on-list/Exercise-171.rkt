;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-171) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constant
;; ========

(define FILE "ttt.txt")
(define LINES (read-lines FILE))    ;; ListOfString
(define WORDS (read-words FILE))    ;; ListOfString

;; Data definitions
;; ================

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of string.

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions
;; =========

(check-expect LINES
              (list "TTT"
                    ""
                    "Put up in a place"
                    "where it's easy to see"
                    "the cryptic admonishment"
                    "T.T.T."
                    ""
                    "When you feel how depressingly"
                    "slowly you climb,"
                    "it's well to remember that"
                    "Things Take Time."
                    ""
                    "Piet Hein"))

(check-expect WORDS
              (list "TTT"
                    "Put" "up" "in" "a" "place"
                    "where" "it's" "easy" "to" "see" "the" "cryptic" "admonishment"
                    "T.T.T."
                    "When" "you" "feel" "how" "depressingly"
                    "slowly" "you" "climb,"
                    "it's" "well" "to" "remember" "that"
                    "Things" "Take" "Time."
                    "Piet" "Hein"))