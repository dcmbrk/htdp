;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-183) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 183

;1)
(check-expect (cons "a" (list 0 #false))
              (cons "a" (cons 0 (cons #false empty))))

(check-expect (cons "a" (list 0 #false))
              (list "a" 0 #false))
;2)
(check-expect (list (cons 1 (cons 13 empty)))
              (cons (cons 1 (cons 13 empty)) empty))

(check-expect (list (cons 1 (cons 13 empty)))
              (list (list 1 13)))
;3)
(check-expect (cons (list 1 (list 13 empty)) empty)
              (cons (cons 1 (cons (cons 13 (cons '() empty)) empty)) empty))

(check-expect (cons (list 1 (list 13 empty)) empty)
              (list (list 1 (list 13 '()))))
;4)
(check-expect (list '() '() (cons 1 '()))
              (cons empty (cons empty (cons (cons 1 empty) empty))))

(check-expect (list '() '() (cons 1 '()))
              (list empty empty (list 1)))
;5)
(check-expect (cons "a" (cons (list 1) (list #false '())))
              (cons "a" (cons (cons 1 empty) (cons #false (cons empty empty)))))

(check-expect (cons "a" (cons (list 1) (list #false '())))
              (list "a" (list 1) #false empty))

