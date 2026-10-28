;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

;; Constant
;; ========

(define ARTICLES (list "a" "an" "the"))   ;LOS

;; Data definitions
;; ================

;; Article is one of:
;; - "a"
;; - "an"
;; - "the"

#;
(define (fn-for-article article)
  (cond [(string=? article "a") (...)]
        [(string=? article "an") (...)]
        [(string=? article "the") (...)]))

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. list of string.

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)                 ;String
              (fn-for-los (rest los)))])) ;LOS

;; Line is ListOfString
;; interp. a line in text file

;; Lines is one of:
;; - emptt
;; (cons Line Lines)
;; interp. a content in text file

#;
(define (fn-for-lines lines)
  (cond [(empty? lines) (...)]
        [else
         (... (fn-for-line (first lines))     ;Line
              (fn-for-lines (rest lines)))])) ;Lines

;; Functions
;; =========

;; String -> ???
;; remove article in given file  

(check-expect (local [(define remove-articles-file (no-articles "ttt.txt"))]
                (read-words "no-articles-ttt.txt"))
              (list
 "TTT"
 "Put" "up" "in" "place"
 "where" "it's" "easy" "to" "see"
 "cryptic" "admonishment"
 "T.T.T."
 "When" "you" "feel" "how" "depressingly"
 "slowly" "you" "climb,"
 "it's" "well" "to" "remember" "that"
 "Things" "Take" "Time." "Piet" "Hein"))

(define (no-articles n)
  (write-file (string-append "no-articles-" n)
              (collapse
               (remove-articles
                (read-words/line n)))))  ;; Lines


;; Lines -> Lines
;; remove article from lines
(check-expect (remove-articles empty) empty)
(check-expect (remove-articles (list
                                (list "TTT")
                                '()
                                (list "Put" "up" "in" "a" "place")
                                (list "where" "it's" "easy" "to" "see")
                                (list "the" "cryptic" "admonishment")
                                (list "T.T.T.")
                                '()
                                (list "When" "you" "feel" "how" "depressingly")
                                (list "slowly" "you" "climb,")
                                (list "it's" "well" "to" "remember" "that")
                                (list "Things" "Take" "Time.")
                                '()
                                (list "Piet" "Hein")))
              (list
               (list "TTT")
               '()
               (list "Put" "up" "in" "place")
               (list "where" "it's" "easy" "to" "see")
               (list "cryptic" "admonishment")
               (list "T.T.T.")
               '()
               (list "When" "you" "feel" "how" "depressingly")
               (list "slowly" "you" "climb,")
               (list "it's" "well" "to" "remember" "that")
               (list "Things" "Take" "Time.")
               '()
               (list "Piet" "Hein")))

#;
(define (remove-articles lines) empty)

(define (remove-articles lines)
  (cond [(empty? lines) empty]
        [else
         (cons (remove-article (first lines))     ;Line
               (remove-articles (rest lines)))])) ;Lines

;; Line -> Line
;; remove article in line.
(check-expect (remove-article empty) empty)
(check-expect (remove-article (list "Put" "up" "in" "a" "place")) (list "Put" "up" "in" "place"))
(check-expect (remove-article (list "the" "cryptic" "admonishment")) (list "cryptic" "admonishment"))

#;
(define (remove-article line) empty)

(define (remove-article line)
  (cond [(empty? line) empty]
        [else
         (if (false? (is-article? (first line)))                     ;String
             (cons (first line) (remove-article (rest line)))
             (remove-article (rest line)))])) ;LOS


;; String -> Boolean
;; Determines if given string is Article
(check-expect (is-article? "") #false)
(check-expect (is-article? "hello") #false)
(check-expect (is-article? "a") #true)
(check-expect (is-article? "an") #true)
(check-expect (is-article? "the") #true)

(define (is-article? string)
  (member? string ARTICLES))

;; ==================================================
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