;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-194) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants
;; =========

(define MT (empty-scene 50 50))
(define triangle-p (list (make-posn 20 10)
                         (make-posn 20 20)
                         (make-posn 30 20)))
(define square-p (list (make-posn 10 10)
                       (make-posn 20 10)
                       (make-posn 20 20)
                       (make-posn 10 20)))
(define COLOR "red")


;; Data definitions
;; ================

;; An NELoP is one of:
;; - (cons Posn '())
;; - (cons Posn NELoP)

;; a Polygon is one of:
;; - (cons Posn (cons Posn (cons Posn '())))
;; - (cons Posn Polygon)
;; Respresent a polygon.

#;
(define (fn-for-polygon p)
  (cond [(empty? (rest (rest (rest p)))) (...)]
        [else
         (... (first p)
              (fn-for-polygon (rest p)))]))

;; Functions
;; =========

;; Image Polygon -> Image
;; renders the given polygon p into img
(check-expect (render-poly MT triangle-p)
              (scene+line
               (scene+line
                (scene+line
                 MT
                 20 10 20 20 COLOR)
                20 20 30 20 COLOR)
               30 20 20 10 COLOR))
(check-expect (render-poly MT square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line
                  MT
                  10 10 20 10 COLOR)
                 20 10 20 20 COLOR)
                20 20 10 20 COLOR)
               10 20 10 10 COLOR))

#;
(define (render-poly img p)
  (connect-dots img (cons (last p) p)))

(define (render-poly img p)
  (connect-dots img p (first p)))

;; Image NELoP Posn -> Image
;; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT (list (make-posn 10 20)) (make-posn 20 20))
              (render-line MT (make-posn 10 20) (make-posn 20 20)))
(check-expect (connect-dots MT triangle-p (first triangle-p))
              (render-line
               (render-line
                (render-line
                 MT
                 (make-posn 20 10) (make-posn 20 20)) 
                (make-posn 20 20) (make-posn 30 20))
               (make-posn 30 20) (make-posn 20 10))) 
#;
(check-expect (connect-dots MT square-p (first square-p))
              (...))

(define (connect-dots img p first-p)
  (cond [(empty? (rest p)) (render-line MT (first p) first-p)]
        [else
         (render-line (connect-dots img (rest p) first-p)
                      (first p) (second p))]))

;; Image Posn Posn -> Image
;; draws a line from posn p to posn q into img
(check-expect (render-line MT (make-posn 20 10) (make-posn 20 20))
              (scene+line MT 20 10 20 20 COLOR))

(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p) 
              (posn-x q) (posn-y q)
              COLOR))