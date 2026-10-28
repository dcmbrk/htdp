;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Poligon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Sample Problem: Design a function that adds a polygon to a given scene.

(require 2htdp/image)

;; Constant
;; ========

(define MT (empty-scene 50 50))
(define LINE-COLOR "black")


;; Data definition
;; ===============

;; Posn is (make-posn Number Number)
;; Represents a point in scene.

#;
(define (fn-for-posn p)
  (... (posn-x p)
       (posn-y p)))

;; Polygon is one of:
;; - (cons Posn (cons Posn (cons Posn empty)))
;; - (cons Posn Polygon)
;; Represent a polygon
;; Constraint: Polygon must has at least 3 Posns

#;
(define (fn-for-polygon p)
  (cond [(empty? (rest (rest (rest p)))) (...)]
        [else
         (... (fn-for-posn (first p))
              (fn-for-polygon (rest p)))]))

;; Functions
;; =========

;; Image Polygon -> Image
;; renders the given polygon p into img
#;
(check-expect (render-poly MT
                           (list (make-posn 20 10)
                                 (make-posn 20 20)
                                 (make-posn 30 20)))
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))

(check-expect (render-poly MT
                           (list (make-posn 20 10)
                                 (make-posn 20 20)
                                 (make-posn 30 20)))
              (render-line
               (render-line
                (render-line 
                 MT
                 (make-posn 20 10)
                 (make-posn 20 20))
                (make-posn 20 20)
                (make-posn 30 20))
               (make-posn 30 20)
               (make-posn 20 10)))

;(define (render-poly img p) empty-image)

(define (render-poly img p)
  (cond [(empty? (rest (rest (rest p)))) (render-line
                                          (render-line
                                           (render-line
                                            img
                                            (first p)
                                            (second p))
                                           (second p)
                                           (third p))
                                          (third p)
                                          (first p))]
        [else
         (render-line (render-poly img (rest p))
                      (first p)
                      (second p))]))


;; Image Posn Posn -> Image
;; ???
;; !!!

(define (render-line img p1 p2)
  (scene+line img
              (posn-x p1) (posn-y p1)
              (posn-x p2) (posn-y p2)
              LINE-COLOR))

(render-poly MT (list (make-posn 10 0)
                      (make-posn 20 0)
                      (make-posn 20 10)
                      (make-posn 10 10)))