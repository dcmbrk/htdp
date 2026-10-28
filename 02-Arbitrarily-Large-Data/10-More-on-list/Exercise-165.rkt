;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-165) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

;; Toy is String
;; interp. name of toy.

;; Toys is one of:
;; - empty
;; - (cons Toy Toys)
;; interp. list of name of toy.

#;
(define (fn-for-toys toys)
  (cond [(empty? toys) (...)]
        [else
         (... (first toys)                  ;; Toy
              (fn-for-toys (rest toys)))])) ;; Toys

;; Functions
;; =========

;; Toys -> Toys
;; Replaces all "robot" with "r2d2", other remain the same.
(check-expect (subst-robot empty) empty)
(check-expect (subst-robot (list "dog" "robot")) (list "dog" "r2d2"))

(define (subst-robot toys)
  (cond [(empty? toys) empty]
        [else
         (cons (replace-robot (first toys))     ;; Toy
               (subst-robot (rest toys)))])) ;; Toys

;; Toy -> Toy
;; Check if toy is "robot" then replace with "r2d2", otherwise remain the same.
(check-expect (replace-robot "dog") "dog")
(check-expect (replace-robot "robot") "r2d2")

(define (replace-robot toy)
  (if (string=? toy "robot")
      "r2d2"
      toy))


;; Toy Toy Toys -> Toys
;; Replace all old toy with new toy.
(check-expect (substitute "robot" "r2d2" empty) empty)
(check-expect (substitute "robot" "r2d2" (list "dog" "robot")) (list "dog" "r2d2"))

;(define (substitute old new toys) empty)

(define (substitute old new toys)
  (cond [(empty? toys) empty]
        [else
         (cons (replace-toy old new (first toys))   ;; Toy
               (substitute old new (rest toys)))])) ;; Toys

;; Toy Toy Toy -> Toy
;; Check if toy is old toy then replace with new toy, otherwise remain the same
(check-expect (replace-toy "robot" "r2d2" "robot") "r2d2")
(check-expect (replace-toy "robot" "r2d2" "dog") "dog")

(define (replace-toy old new toy)
  (if (string=? toy old)
      new
      toy))
