;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-250) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ===============
;; Data definition

;; NATURAL is one of:
;; - 0
;; - (add1 NATURAL)
;; represent a natural number.

(define N0 0)
(define N1 (add1 N0))
(define N2 (add1 N1))

#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (... n
              (fn-for-natural (sub1 n)))]))

;; =========
;; Functions

;; Number -> ListOfNumber
;; tabulates sin between n and 0 in a list.
(check-within (tab-sin 0) (list (sin 0)) 0.001)
(check-within (tab-sin 1) (list (sin 1) (sin 0)) 0.001)
(check-within (tab-sin 2) (list (sin 2) (sin 1) (sin 0)) 0.001)

#;
(define (tab-sin n) empty)

#;
(define (tab-sin n)
  (cond [(zero? n) (cons (sin n) empty)]
        [else
         (cons (sin n)
               (tab-sin (sub1 n)))]))

(define (tab-sin n)
  (natural-fold n sin))

;; Number -> ListOfNumber
;; tabulates sqrt between n and 0 in a list.
(check-within (tab-sqrt 0) (list (sqrt 0)) 0.001)
(check-within (tab-sqrt 1) (list (sqrt 1) (sqrt 0)) 0.001)
(check-within (tab-sqrt 2) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

#;
(define (tab-sqrt n) empty)

#;
(define (tab-sqrt n)
  (cond [(zero? n) (cons (sqrt n) empty)]
        [else
         (cons (sqrt n)
               (tab-sqrt (sub1 n)))]))

(define (tab-sqrt n)
  (natural-fold n sqrt))

;; Number [Number -> Number] -> ListOfNumber
;; natural-fold function.
(check-within (natural-fold 2 sin) (list (sin 2) (sin 1) (sin 0)) 0.001)
(check-within (natural-fold 2 sqrt) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

(define (natural-fold n f)
  (cond [(zero? n) (cons (f n) empty)]
        [else
         (cons (f n)
               (natural-fold (sub1 n) f))]))
