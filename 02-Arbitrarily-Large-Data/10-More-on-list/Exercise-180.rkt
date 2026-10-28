;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-180) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constant
;; ========

(define FONT-SIZE 24)
(define FONT-COLOR "black")

;; Data definitions
;; ================

(define-struct editor [pre post])
;; Editor is (make-editor LO1S LO1S)

#;
(define (fn-for-editor ed)
  (... (fn-for-lo1s (editor-pre ed))
       (fn-for-lo1s (editor-post ed))))

;; LO1S is one of:
;; - empty
;; - (cons 1String LO1S)

#;
(define (fn-for-lo1s lo1s)
  (cond [(empty? lo1s) (...)]
        [else
         (... (first lo1s)
              (fn-for-lo1s (rest lo1s)))]))

;; Functions
;; =========

;; LO1S -> Image
;; ...
(check-expect (editor-text (list "a" "b"))
              (text "ab" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (text (implode* s) FONT-SIZE FONT-COLOR))

;; LO1S -> String
;; ...
(check-expect (implode* empty) "")
(check-expect (implode* (list "a" "b")) "ab")

;(define (implode* s) "")

(define (implode* lo1s)
  (cond [(empty? lo1s) ""]
        [else
         (string-append (first lo1s)
                        (implode* (rest lo1s)))]))