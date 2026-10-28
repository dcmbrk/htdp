;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-215) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Spanw position
(define SPAWN-POSITION (make-posn CENTER-X CENTER-Y))

;; Data definitions
;; ================

(define-struct worm
  [velocity position segments])
;; Worm is (make-worm Posn (listof Segment))
;; Represents an instance of worm with: velocity is direction that worm moving, position of worm in world, segments is body of worms.

(define WORM-LEFT 
  (make-worm LEFT
             SPAWN-POSITION
             (list SEGMENT)))
(define WORM-RIGHT
  (make-worm RIGHT
             SPAWN-POSITION
             (list SEGMENT)))
(define WORM-UP
  (make-worm UP
             SPAWN-POSITION
             (list SEGMENT)))
(define WORM-DOWN
  (make-worm DOWN
             SPAWN-POSITION
             (list SEGMENT)))

;; Functions
;; =========

;; Worm -> Worm
;; start the world with ...
;; !!!

(define (main w)
  (big-bang w
    (on-tick tock)
    (to-draw render)
    (on-key key-handler)))

;; Worm -> Worm
;; procudes next worm's state. 
(check-expect (tock WORM-LEFT)
              (make-worm (worm-velocity WORM-LEFT)
                         (make-posn (+ (posn-x (worm-position WORM-LEFT)) (* (posn-x (worm-velocity WORM-LEFT)) WORM-SPEED))
                                    (+ (posn-y (worm-position WORM-LEFT)) (* (posn-y (worm-velocity WORM-LEFT)) WORM-SPEED)))
                         (worm-segments WORM-LEFT)))
(check-expect (tock WORM-RIGHT)
              (make-worm (worm-velocity WORM-RIGHT)
                         (make-posn (+ (posn-x (worm-position WORM-RIGHT)) (* (posn-x (worm-velocity WORM-RIGHT)) WORM-SPEED))
                                    (+ (posn-y (worm-position WORM-RIGHT)) (* (posn-y (worm-velocity WORM-RIGHT)) WORM-SPEED)))
                         (worm-segments WORM-RIGHT)))
(check-expect (tock WORM-UP)
              (make-worm (worm-velocity WORM-UP)
                         (make-posn (+ (posn-x (worm-position WORM-UP)) (* (posn-x (worm-velocity WORM-UP)) WORM-SPEED))
                                    (+ (posn-y (worm-position WORM-UP)) (* (posn-y (worm-velocity WORM-UP)) WORM-SPEED)))
                         (worm-segments WORM-UP)))
(check-expect (tock WORM-DOWN)
              (make-worm (worm-velocity WORM-DOWN)
                         (make-posn (+ (posn-x (worm-position WORM-DOWN)) (* (posn-x (worm-velocity WORM-DOWN)) WORM-SPEED))
                                    (+ (posn-y (worm-position WORM-DOWN)) (* (posn-y (worm-velocity WORM-DOWN)) WORM-SPEED)))
                         (worm-segments WORM-DOWN)))

(define (tock w)
  (make-worm (worm-velocity w)
             (make-posn (+ (posn-x (worm-position w)) (* (posn-x (worm-velocity w)) WORM-SPEED))
                        (+ (posn-y (worm-position w)) (* (posn-y (worm-velocity w)) WORM-SPEED)))
             (worm-segments w)))

;; Worm -> Image
;; render worm in scene.
(check-expect (render WORM-LEFT)
              (place-image SEGMENT CENTER-X CENTER-Y MTS))

(define (render w)
  (place-image (first (worm-segments w))
               (posn-x (worm-position w))
               (posn-y (worm-position w))
               MTS))

;; Worm KeyEvent -> Worm
;; handle worm input.
(check-expect (key-handler WORM-RIGHT "left")
              (make-worm LEFT
                         (worm-position WORM-RIGHT)
                         (worm-segments WORM-RIGHT)))
(check-expect (key-handler WORM-LEFT "right")
              (make-worm RIGHT
                         (worm-position WORM-LEFT)
                         (worm-segments WORM-LEFT)))
(check-expect (key-handler WORM-UP "down")
              (make-worm DOWN
                         (worm-position WORM-UP)
                         (worm-segments WORM-UP)))
(check-expect (key-handler WORM-DOWN "up")
              (make-worm UP
                         (worm-position WORM-DOWN)
                         (worm-segments WORM-DOWN)))
(check-expect (key-handler WORM-RIGHT "a")
              WORM-RIGHT)

(define (key-handler w ke)
  (cond [(key=? ke "left")
         (make-worm LEFT
                    (worm-position w)
                    (worm-segments w))]
        [(key=? ke "right")
         (make-worm RIGHT
                    (worm-position w)
                    (worm-segments w))]
        [(key=? ke "up")
         (make-worm UP
                    (worm-position w)
                    (worm-segments w))]
        [(key=? ke "down")
         (make-worm DOWN
                    (worm-position w)
                    (worm-segments w))]
        [else w]))

;; Application
;; ===========

;(main WORM-RIGHT)
