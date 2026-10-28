;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-214) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


;; Word -> (listof Word)
;; creates all rearrangements of the letters in w.
(check-expect (arrangements (list "d" "e"))
              (list (list "d" "e")
                    (list "e" "d")))

#;
(define (arrangements w) empty)

(define (arrangements w)
  (cond [(empty? w) (list empty)]
        [else
         (insert-everywhere/in-all-words (first w) (rest w))]))

;; 1String Word -> (listof Word)
;; insert 1s at beginning, between all letters, and at the end.
(check-expect (insert-everywhere/in-all-words "d" empty)
              (list (list "d")))
(check-expect (insert-everywhere/in-all-words "d" (list "e"))
              (list (list "d" "e")
                    (list "e" "d")))
(check-expect (insert-everywhere/in-all-words "c" (list "a" "t"))
              (list (list "c" "a" "t")
                    (list "a" "t" "c")
                    (list "a" "c" "t")))

#;
(define (insert-everywhere/in-all-words 1s w) empty)

(define (insert-everywhere/in-all-words 1s w)
  (cond [(empty? w) (list (list 1s))]
        [(empty? (rest w)) (list (insert-beginning 1s w)
                                 (insert-at-end 1s w))]
        [else
         (cons (insert-beginning 1s w)
               (cons (insert-at-end 1s w)
                     (insert-between 1s w)))]))


;; 1String Word -> Word
;; insert 1s at beginning.
(check-expect (insert-beginning "d" empty) (list "d"))
(check-expect (insert-beginning "d" (list "e")) (list "d" "e"))

#;
(define (insert-beginning 1s w) empty)

(define (insert-beginning 1s w)
  (cons 1s w))

;; 1String Word -> Word
;; insert 1s at end.
(check-expect (insert-at-end "d" empty) (list "d"))
(check-expect (insert-at-end "d" (list "e")) (list "e" "d"))

#;
(define (insert-at-end 1s w) empty)

(define (insert-at-end 1s w)
  (cond [(empty? w) (cons 1s empty)]
        [else
         (cons (first w)  
               (insert-at-end 1s (rest w)))]))

;; 1String Word -> (listof Word)
;; insert 1s at between.
(check-expect (insert-between "c" empty)
              (list (list "c")))
(check-expect (insert-between "h" (list "c" "a" "t"))
              (list (list "c" "h" "a" "t")
                    (list "c" "a" "h" "t")))

#;
(define (insert-between 1s w) empty)

(define (insert-between 1s w)
  (cond [(empty? w) (list (list 1s))]
        [else
         (insert-between# 1s (list (first w)) (rest w))]))


;; 1String Word Word -> (listof Word)
;; insert 1s at between#.
(check-expect (insert-between# "h" (list "c") (list "a" "t"))
              (list (list "c" "h" "a" "t")
                    (list "c" "a" "h" "t")))

#;
(define (insert-between# 1s prefix suffix) empty)

(define (insert-between# 1s prefix suffix)
  (cond [(empty? suffix) empty]
        [else
         (cons (insert-between-fix 1s prefix suffix)
               (insert-between# 1s
                                (append prefix (list (first suffix)))
                                (rest suffix)))]))

;; 1String Word Word -> Word
;; insert 1s beetween prefix and suffix.
(check-expect (insert-between-fix "h" (list "c") (list "a" "t"))
              (list "c" "h" "a" "t"))
(check-expect (insert-between-fix "h" (list "c" "a") (list "t"))
              (list "c" "a" "h" "t"))

#;
(define (insert-between-fix 1s prefix suffix) empty)

(define (insert-between-fix 1s prefix suffix)
  (append prefix (list 1s) suffix))


;; String -> List-of-strings
;; finds all words that use the same letters as s
(check-member-of (alternative-words "cat") 
                 (list "act" "cat")
                 (list "cat" "act"))

(define (alternative-words s)
  (words->strings
   (arrangements
    (string->word s))))
