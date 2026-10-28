;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


;; String is ListOf1String

#;
(define (fn-for-string s)
  (local [(define (fn-for-string s acc)
            (cond [(= (string-length s) acc) ""]
                  [else
                   (string-append (fn-for-1string (substring s acc (add1 acc)))
                                  (fn-for-string s (add1 acc)))]))]
    (fn-for-string s 0)))

;; Functions
;; =========

;; String -> String
;; Consumes a file name and produces "[lines] [words] [characters] [filename]"
(check-expect (wc "ttt.txt") "13 33 183 ttt.txt")

(define (wc n)
  (string-append (number->string (length (read-lines n))) " "
                 (number->string (length (read-words n))) " "
                 (number->string (string-length (read-file n))) " "
                 n))
