;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

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

;; Lines is one of:
;; - empty
;; - (cons ListOfString Lines)
;; interp. a list of line. with line is list of string.

#;
(define (fn-for-lines lines)
  (cond [(empty? lines) (...)]
        [else
         (... (fn-for-los (first lines))
              (fn-for-lines (rest lines)))]))

;; Functions
;; =========

;; Lines -> String
;; Append word with " " and append line with "\n".
(check-expect (collapse empty) "")
(check-expect (collapse (list (list "Hello" "world")
                              (list "How" "are" "you")))
              "Hello world\nHow are you")

(define (collapse lines)
  (cond [(empty? lines) ""]
        [(empty? (rest lines)) (concat (first lines))]
        [else
         (write-line (concat (first lines))        ;ListOfString
                     (collapse (rest lines)))]))   ;Lines

;; ListOfString -> String
;; concat words
(check-expect (concat (list "Hello" "world")) "Hello world")
(check-expect (concat empty) "")

(define (concat los)
  (cond [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [else
         (string-append (first los)
                        " "
                        (concat (rest los)))]))

;; String String -> String
;; add \n between string
(check-expect (write-line "Hello world" "How are you")
              "Hello world\nHow are you")

(define (write-line s1 s2)
  (string-append s1 "\n" s2))


(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))