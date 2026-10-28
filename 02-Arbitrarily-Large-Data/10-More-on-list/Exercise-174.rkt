;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; String -> ...
;; Decode the given file with new file name "encoded-n"
(check-expect (local [(define encoded-file (encode-file "abc.txt"))]
                (read-words/line "encoded-abc.txt"))
              (list (list "097098")
                    (list "099")))

(define (encode-file n)
  (write-file (string-append "encoded-" n)  ;File String -> ...
              (collapse                     ;Lines -> String
               (encode-lines                ;Lines -> Lines
                (read-words/line n)))))     ;File  -> Lines


;; Lines -> Lines
;; Encoding lines to numbers.
(check-expect (encode-lines empty) empty)
(check-expect (encode-lines (list (list "a" "b")
                                  (list "c")))
              (list (list "097" "098")
                    (list "099")))

(define (encode-lines lines)
  (cond [(empty? lines) empty]
        [else
         (cons (encode-line (first lines))      ; ListOfString
               (encode-lines (rest lines)))]))  ; Lines

;; Line -> Line
;; Encoding line to number.
(check-expect (encode-line empty) empty)
(check-expect (encode-line (list "a" "b")) (list "097" "098"))

(define (encode-line los)
  (cond [(empty? los) empty]
        [else
         (cons (encode-word (first los))
               (encode-line (rest los)))]))

;; String -> String
;; Encoding word to numbers.
(check-expect (encode-word "") "")
(check-expect (encode-word "ab") "097098")

#;
(define (encode-word word) word)

(define (encode-word word)
  (local [(define (encode-word word acc)
            (cond [(= (string-length word) acc) ""]
                  [else
                   (string-append (encode-letter (substring word acc (add1 acc)))
                                  (encode-word word (add1 acc)))]))]
    (encode-word word 0)))

;=================================================
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



;=========================================

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))