;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Execise-217) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
;; =========

;; Polygon
(define SIZE-LENGTH 10)
(define SIZE-COUNT 8)
(define OUTLINE-MODE "solid")
(define SEGMENT-COLOR "red")

;; Segment
(define SEGMENT (regular-polygon SIZE-LENGTH SIZE-COUNT OUTLINE-MODE SEGMENT-COLOR))

;; Display
(define WIDTH 600)
(define HEIGHT 400)
(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))

;; Worm
(define WORM-SPEED SIZE-LENGTH)

;; INPUT
(define LEFT (make-posn -1 0))
(define RIGHT (make-posn 1 0))
(define UP (make-posn 0 -1))
(define DOWN (make-posn 0 1))

;; Spawn position
(define SPAWN-POSITION (make-posn CENTER-X CENTER-Y))

;; Message
(define MESSAGE-WALL-CONTENT "worm hit border")
(define MESSAGE-SELF-CONTENT "worm hit self")
(define MESSAGE-SIZE 16)
(define MESSAGE-COLOR "black")
(define MESSAGE-X CENTER-X)
(define MESSAGE-Y (- HEIGHT 30))

(define END-GAME-WALL-MESSAGE
  (place-image (text MESSAGE-WALL-CONTENT MESSAGE-SIZE MESSAGE-COLOR) MESSAGE-X MESSAGE-Y MTS))
(define END-GAME-SELF-MESSAGE
  (place-image (text MESSAGE-SELF-CONTENT MESSAGE-SIZE MESSAGE-COLOR) MESSAGE-X MESSAGE-Y MTS))
 
;; Data definitions
;; ================

;; Segment is Posn
;; Represents a position of segment in world

(define HEAD (make-posn CENTER-X CENTER-Y))
(define BODY-1 (make-posn (- (posn-x HEAD) (* 2 SIZE-LENGTH))
                          (posn-y HEAD)))
(define BODY-2 (make-posn (- (posn-x HEAD) (* 4 SIZE-LENGTH))
                          (posn-y HEAD)))
(define BODY-3 (make-posn (- (posn-x HEAD) (* 6 SIZE-LENGTH))
                          (posn-y HEAD)))
(define BODY-4 (make-posn (- (posn-x HEAD) (* 8 SIZE-LENGTH))
                          (posn-y HEAD)))

;; (listof Segment) is one of:
;; - empty
;; - (cons Segment (listof Segment))
;; Represents a list of segment.

#;
(define (fn-for-segments segments)
  (cond [(empty? segments) (...)]
        [else
         (... (fn-for-segment (first segments))
              (fn-for-segments (rest segments)))]))


(define-struct worm
  [velocity segments])
;; Worm is (make-worm (listof Segment))
;; Represents an instance of worm with: velocity is direction that worm moving, position of worm in world, segments is body of worms.

(define WORM-LEFT 
  (make-worm LEFT (list HEAD)))
(define WORM-RIGHT
  (make-worm RIGHT (list HEAD)))
(define WORM-UP
  (make-worm UP (list HEAD)))
(define WORM-DOWN
  (make-worm DOWN (list HEAD)))

(define WORM
  (make-worm RIGHT (list HEAD BODY-1 BODY-2 BODY-3 BODY-4)))

#;
(define (fn-for-worm w)
  (... (fn-for-posn (worm-velocity w))
       (fn-for-segments (worm-segments w))))

;; Functions
;; =========

;; Worm -> Worm
;; start the world with ...
;; !!!

(define (main w)
  (big-bang w
    (on-tick tock)
    (to-draw render)
    (on-key key-handler)
    (stop-when endgame? render-message)))

;; Worm -> Worm
;; procudes next worm's state.
(check-expect (tock (make-worm RIGHT
                               (list HEAD BODY-1 BODY-2)))
              (make-worm RIGHT
                         (list (make-posn (+ (posn-x HEAD) (* 2 SIZE-LENGTH))
                                          (posn-y HEAD))
                               HEAD
                               BODY-1)))

#;
(define (tock w) w)

(define (tock w)
  (make-worm (worm-velocity w)
             (next-segments (worm-velocity w)
                            (worm-segments w))))

;; Posn Segments -> Segments
;; produces next segments.
(check-expect (next-segments RIGHT (list HEAD))
              (list (make-posn (+ (posn-x HEAD) (* 2 SIZE-LENGTH))
                               (posn-y HEAD))))
(check-expect (next-segments RIGHT (list HEAD BODY-1 BODY-2))
              (list (make-posn (+ (posn-x HEAD) (* 2 SIZE-LENGTH))
                               (posn-y HEAD))
                    HEAD
                    BODY-1))
#;
(define (next-segments velocity segments) empty)

(define (next-segments velocity segments)
  (cons (make-posn (+ (posn-x (first segments)) (* (posn-x velocity) (* 2 SIZE-LENGTH)))
                   (+ (posn-y (first segments)) (* (posn-y velocity) (* 2 SIZE-LENGTH))))
        (remove-last segments)))

;; (listof X) -> (listof X)
;; remove last item.
(check-expect (remove-last empty) empty)
(check-expect (remove-last (list "x")) empty)
(check-expect (remove-last (list "x" "y")) (list "x"))

#;
(define (remove-last lox) empty)

(define (remove-last lox)
  (cond [(empty? lox) empty]
        [(empty? (rest lox)) empty]
        [else
         (cons (first lox)
               (remove-last (rest lox)))]))

;; Worm -> Image
;; render worm in scene.
(check-expect (render WORM-LEFT)
              (place-image SEGMENT CENTER-X CENTER-Y MTS))

#;
(define (render w) MTS)

(define (render w)
  (segments->image (worm-segments w)))

;; (listof Segment) -> Image
;; render segments of worm. 
(check-expect (segments->image (list HEAD))
              (place-image SEGMENT (posn-x HEAD) (posn-y HEAD) MTS))
(check-expect (segments->image (list HEAD BODY-1 BODY-2))
              (place-image SEGMENT (posn-x BODY-2) (posn-y BODY-2)
                           (place-image SEGMENT (posn-x BODY-1) (posn-y BODY-1)
                                        (place-image SEGMENT (posn-x HEAD) (posn-y HEAD) MTS))))

#;
(define (segments->image segments) MTS)

(define (segments->image segments)
  (cond [(empty? segments) MTS]
        [else
         (place-image SEGMENT (posn-x (first segments)) (posn-y (first segments))
                      (segments->image (rest segments)))]))

;; Worm KeyEvent -> Worm
;; handle worm input.
(check-expect (key-handler WORM-RIGHT "left")
              (make-worm LEFT
                         (worm-segments WORM-RIGHT)))
(check-expect (key-handler WORM-LEFT "right")
              (make-worm RIGHT
                         (worm-segments WORM-LEFT)))
(check-expect (key-handler WORM-UP "down")
              (make-worm DOWN
                         (worm-segments WORM-UP)))
(check-expect (key-handler WORM-DOWN "up")
              (make-worm UP
                         (worm-segments WORM-DOWN)))
(check-expect (key-handler WORM-RIGHT "a")
              WORM-RIGHT) 

(define (key-handler w ke)
  (cond [(key=? ke "left")
         (make-worm LEFT
                    (worm-segments w))]
        [(key=? ke "right")
         (make-worm RIGHT
                    (worm-segments w))]
        [(key=? ke "up")
         (make-worm UP
                    (worm-segments w))]
        [(key=? ke "down")
         (make-worm DOWN
                    (worm-segments w))]
        [else w]))


;; Worm -> Boolean
;; determinse if end game.

(define (endgame? w)
  (or (worm-hit-wall? w)
      (worm-hit-self? w)))

;; Worm -> Boolean
;; stop worm when hit wall.
(check-expect (worm-hit-wall? WORM-LEFT) #false)
(check-expect (worm-hit-wall?
               (make-worm RIGHT
                          (list (make-posn (+ WIDTH WORM-SPEED)
                                     CENTER-Y))))
              #true)
(check-expect (worm-hit-wall?
               (make-worm DOWN
                          (list (make-posn CENTER-X
                                     (+ HEIGHT WORM-SPEED)))))
              #true)


(define (worm-hit-wall? w)
  (or (> (posn-x (first (worm-segments w))) WIDTH)
      (< (posn-x (first (worm-segments w))) 0)
      (> (posn-y (first (worm-segments w))) HEIGHT)
      (< (posn-y (first (worm-segments w))) 0)))

;; Worm -> Boolean
;; stop when worm hit it self.
(check-expect (worm-hit-self? WORM) #false)
(check-expect (worm-hit-self? (make-worm RIGHT
                                         (list BODY-3 BODY-1 BODY-2 BODY-3)))
              #true)

(define (worm-hit-self? w)
  (member? (first (worm-segments w))
           (rest (worm-segments w))))

;; Worm -> Image
;; render end game scene.
(check-expect (render-message WORM-LEFT) MTS)
(check-expect (render-message (make-worm DOWN
                          (list (make-posn CENTER-X
                                     (+ HEIGHT WORM-SPEED)))))
              (place-image (text MESSAGE-WALL-CONTENT MESSAGE-SIZE MESSAGE-COLOR) MESSAGE-X MESSAGE-Y MTS))
(check-expect (render-message (make-worm RIGHT
                                         (list BODY-3 BODY-1 BODY-2 BODY-3)))
              (place-image (text MESSAGE-SELF-CONTENT MESSAGE-SIZE MESSAGE-COLOR) MESSAGE-X MESSAGE-Y MTS))

(define (render-message w)
  (cond [(worm-hit-wall? w) END-GAME-WALL-MESSAGE]
        [(worm-hit-self? w) END-GAME-SELF-MESSAGE]
        [else MTS]))


;; Application
;; ===========

;(main WORM)
