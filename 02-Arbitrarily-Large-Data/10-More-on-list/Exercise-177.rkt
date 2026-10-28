;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-177) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

(define-struct editor [pre post])
;; Editor is (make-editor String String)

;; Functions
;; =========

;; String String -> Editor
;; Create instance of editor
(check-expect (create-editor "" "") (make-editor "" ""))
(check-expect (create-editor "ab" "") (make-editor "ab" ""))
(check-expect (create-editor "" "cd") (make-editor "" "cd"))
(check-expect (create-editor "ab" "cd") (make-editor "ab" "cd"))

(define (create-editor pre post)
  (make-editor pre post))

