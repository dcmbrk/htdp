;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-242) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ===============
;; Data defintions

;; ListOfString is one of:
;; - empty
;; (cons String ListOfString)
;; Represents a list of string.

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; [MaybeX] is one of:
;; - X
;; - #false
;; Represents a data that can be X or #false

#;
(define (fn-for-mx mx)
  (cond [(false? mx) (...)]
        [else
         (... (fn-for-x mx))]))

;; MaybeString is [Maybe String]
;; Represents a data that can be String of #fasle

(define MS-F #false)
(define MS-1 "hello")

#;
(define (fn-for-ms ms)
  (cond [(false? ms) (...)]
        [else
         (... (fn-for-s ms))]))

;; MaybeListOfString is [Maybe ListOfString]
;; Represents a data that can be ListOfString or #false

(define MLOS-F #false)
(define MLOS-1 empty)
(define MLOS-2 (cons "hello" empty))

#;
(define (fn-for-mlos mlos)
  (cond [(false? mlos) (...)]
        [else
         (... (fn-for-los mlos))]))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; =========
;; Functions

;; String ListOfString -> MaybeListOfString
;; returns the remainder of los strting with s.
(check-expect (occurs "a" (list "b" "a" "d" "e")) (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #false)

#;
(define (occurs s los) #false)

(define (occurs s los)
  (cond [(empty? los) #false]
        [else
         (if (string=? (first los) s)
             (rest los)
             (occurs s (rest los)))]))
