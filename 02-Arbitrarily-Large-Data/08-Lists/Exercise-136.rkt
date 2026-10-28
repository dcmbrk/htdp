;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-136) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

(define-struct pair [left right])
;; Pair is (make-pair Any Any)
;; Represents a pair of values

(define P0 (make-pair '() '()))
(define P1 (make-pair "A" '()))
(define P2 (make-pair '() "A"))
(define P3 (make-pair "A" "A"))

;; CONS is one of
;; - '()
;; - (make-pair Any CONS)
;; Represent cons expression

(define EMPTY '())
(define C1 (make-pair "A" EMPTY))
(define C2 (make-pair "A" (make-pair "B" EMPTY)))

#;
(define (fn-for-cons c)
  (cond [(empty? c) (...)]
        [else
         (... (pair-left c)
              (fn-for-cons (pair-right c)))]))

;; Template rules used:
;; - one of 2 cases:
;; - atomic distinct: '()
;; - compound: (make-pair Any Any)

;; Any Any -> CONS
;; produce cons expression
(check-error (CONS "A" "A") "CONS: second argument must be empty or another CONS")
(check-expect (CONS "A" EMPTY) (make-pair "A" EMPTY))
(check-expect (CONS "A" (CONS "B" EMPTY)) (make-pair "A" (make-pair "B" EMPTY)))

;(define (CONS first rest) '())

(define (CONS first rest)
  (cond [(or (empty? rest) (pair? rest)) (make-pair first rest)]
        [else (error "CONS: second argument must be empty or another CONS")]))


;; CONS -> Any
;; produce first expresion
(check-error (FIRST EMPTY) "FIRST: expects a non-empty list")
(check-expect (FIRST (CONS "A" EMPTY)) "A")
(check-expect (FIRST (CONS "A" (CONS "B" EMPTY))) "A")

(define (FIRST lst)
  (cond [(empty? lst) (error "FIRST: expects a non-empty list")]
        [else (pair-left lst)]))

;; CONS -> Any
;; produce rest expression
(check-error (REST EMPTY) "REST: expects a non-empty list")
(check-expect (REST (CONS "A" EMPTY)) EMPTY)
(check-expect (REST (CONS "A" (CONS "B" EMPTY))) (CONS "B" EMPTY))

;(define (REST lst) "")

(define (REST lst)
  (cond [(empty? lst) (error "REST: expects a non-empty list")]
        [else (pair-right lst)]))



(FIRST (CONS "a" '()))

(REST (CONS "a" '()))