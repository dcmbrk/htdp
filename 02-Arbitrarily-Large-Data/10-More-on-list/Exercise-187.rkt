;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 187

;; Data definitions
;; ================

(define-struct gp [name score])
;; GamePlayer is (make-gp String Number)
;; represents a player with name and score.

#;
(define (fn-for-gp gp)
  (... (gp-name gp)
       (gp-score gp)))

;; GamePlayer/s is one of:
;; - empty
;; - (cons GamePlayer GamePlayer/s)
;; represents a list of game player.

#;
(define (fn-for-gps gps)
  (cond [(empty? gps) (...)]
        [else
         (... (fn-for-gp (first gps))
              (fn-for-gps (rest gps)))]))

;; Functions
;; =========

;; GamePlayer/s -> Boolean
;; Determines if list of game player are sorted.
(check-expect (sorted>? empty) #true)
(check-expect (sorted>? (list (make-gp "P1" 100))) #true)
(check-expect (sorted>? (list (make-gp "P2" 200)
                                   (make-gp "P1" 100)))
              #true)
(check-expect (sorted>? (list (make-gp "P1" 100)
                                   (make-gp "P2" 200)))
              #false)

(define (sorted>? gps)
  (cond [(empty? gps) #true]
        [(empty? (rest gps)) #true]
        [else
         (and (> (gp-score (first gps))
                 (gp-score (second gps)))
              (sorted>? (rest gps)))]))

;; GamePlayer/s -> GamePlayer/s
;; Inserting sort for game player list.
(check-expect (sorting empty) empty)
(check-expect (sorting (list (make-gp "P1" 100)))
              (list (make-gp "P1" 100)))
(check-expect (sorting (list (make-gp "P1" 100)
                                  (make-gp "P2" 200)))
              (list (make-gp "P2" 200)
                    (make-gp "P1" 100)))
(check-expect (sorting (list (make-gp "P2" 200)
                                  (make-gp "P1" 100)))
              (list (make-gp "P2" 200)
                    (make-gp "P1" 100)))
(check-expect (sorting (list (make-gp "P3" 300)
                                  (make-gp "P1" 100)
                                  (make-gp "P2" 200)))
              (list (make-gp "P3" 300)
                    (make-gp "P2" 200)
                    (make-gp "P1" 100)))

(define (sorting gps)
  (cond [(empty? gps) empty]
        [else
         (insert (first gps)
                 (sorting (rest gps)))]))

;; GamePlayer GamePlayer/s -> GamePlayer/s
;; insert gameplayer to sorted gameplayer list.
(check-expect (insert (make-gp "P1" 100) empty) (list (make-gp "P1" 100)))
(check-expect (insert (make-gp "P1" 100)
                      (list (make-gp "P2" 200)))
              (list (make-gp "P2" 200)
                    (make-gp "P1" 100)))
(check-expect (insert (make-gp "P2" 200)
                      (list (make-gp "P1" 100)))
              (list (make-gp "P2" 200)
                    (make-gp "P1" 100)))
(check-expect (insert (make-gp "P2" 200)
                      (list (make-gp "P3" 300)
                            (make-gp "P1" 100)))
              (list (make-gp "P3" 300)
                    (make-gp "P2" 200)
                    (make-gp "P1" 100)))


(define (insert gp gps)
  (cond [(empty? gps) (cons gp gps)]
        [else
         (if (> (gp-score gp) (gp-score (first gps)))
             (cons gp gps)
             (cons (first gps) (insert gp (rest gps))))]))
