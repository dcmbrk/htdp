;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-219) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
;; =========

;; Polygon
(define SIZE-LENGTH 10)
(define SIZE-COUNT 8)
(define OUTLINE-MODE "solid")
(define SEGMENT-COLOR "red")
(define FOOD-COLOR "green")

;; Segment
(define SEGMENT (regular-polygon SIZE-LENGTH SIZE-COUNT OUTLINE-MODE SEGMENT-COLOR))

;; Food
(define FOOD-IMG (regular-polygon SIZE-LENGTH SIZE-COUNT OUTLINE-MODE FOOD-COLOR))

;; Display
(define WIDTH (* 30 (image-width SEGMENT)))
(define HEIGHT (* 24 (image-width SEGMENT)))
(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT "transparent"))

;; Worm
(define WORM-SPEED (/ (image-width SEGMENT) 4))

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
(define BODY-1 (make-posn (- (posn-x HEAD) (* 2 (image-width SEGMENT)))
                          (posn-y HEAD)))
(define BODY-2 (make-posn (- (posn-x HEAD) (* 4 (image-width SEGMENT)))
                          (posn-y HEAD)))
(define BODY-3 (make-posn (- (posn-x HEAD) (* 6 (image-width SEGMENT)))
                          (posn-y HEAD)))
(define BODY-4 (make-posn (- (posn-x HEAD) (* 8 (image-width SEGMENT)))
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


;; Food is (make-posn Natural[0, WIDTH] Natural[0, HEIGHT])
(define FOOD (make-posn (* (image-width SEGMENT) 3)
                        (* (image-width SEGMENT) 5)))

;; Represent a food in game.

(define-struct game-state
  [worm food])
;; GameState is (make-game-state Worm Food)
;; Represent a current game state.

#;
(define (fn-for-game-state gs)
  (... (fn-for-worm (game-state-worm gs))    ;Worm
       (fn-for-posn (game-state-food gs))))  ;Food / Posn

;; Functions
;; =========

;; GameState -> GameState
;; start the world with ...
;; !!!

(define (main w)
  (big-bang w
    (on-tick tock-game)
    (to-draw render-game)
    (on-key game-key-handler)
    (stop-when endgame? render-message)))

;; 

;; Worm -> Worm
;; procudes next worm's state.
(check-expect (tock (make-worm RIGHT
                               (list HEAD BODY-1 BODY-2)))
              (make-worm RIGHT
                         (list (make-posn (+ (posn-x HEAD) (* 2 WORM-SPEED))
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
              (list (make-posn (+ (posn-x HEAD) (* 2 WORM-SPEED))
                               (posn-y HEAD))))
(check-expect (next-segments RIGHT (list HEAD BODY-1 BODY-2))
              (list (make-posn (+ (posn-x HEAD) (* 2 WORM-SPEED))
                               (posn-y HEAD))
                    HEAD
                    BODY-1))
#;
(define (next-segments velocity segments) empty)

(define (next-segments velocity segments)
  (cons (make-posn (+ (posn-x (first segments)) (* (posn-x velocity) (* 2 WORM-SPEED)))
                   (+ (posn-y (first segments)) (* (posn-y velocity) (* 2 WORM-SPEED))))
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
(check-expect (render-worm WORM-LEFT)
              (place-image SEGMENT CENTER-X CENTER-Y MTS))

#;
(define (render-worm w) MTS)

(define (render-worm w)
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


;; GameState -> Boolean.
;; determinse if end game.
(define (endgame? gs)
  (or (worm-hit-wall? (game-state-worm gs))
      (worm-hit-self? (game-state-worm gs))))


;; Worm -> Boolean
;; determinse if end game.

#;
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

;; Food -> Image
;; render food.
(check-expect (render-food FOOD) (place-image FOOD-IMG (posn-x FOOD) (posn-y FOOD) MTS))

#;
(define (render-food food) MTS)

(define (render-food food)
  (place-image FOOD-IMG (posn-x food) (posn-y food) MTS))


;; GameState -> Image
;; if worm eat food, generate new food otherwise move worm.
(check-expect (render-game (make-game-state WORM-RIGHT FOOD))
              (overlay (render-worm WORM-RIGHT)
                       (render-food FOOD)))

#;
(define (render-game gs) MTS)

(define (render-game gs)
  (overlay (render-worm (game-state-worm gs))
           (render-food (game-state-food gs))))

;; GameState -> GameState
;; render worm and food.
(check-expect (tock-game (make-game-state
                          WORM-RIGHT
                          (make-posn CENTER-X CENTER-Y)))
              (generate-food (make-game-state
                          WORM-RIGHT
                          (make-posn CENTER-X CENTER-Y))))
(check-expect (tock-game (make-game-state
                          WORM-RIGHT
                          (make-posn WIDTH HEIGHT)))
              (make-game-state
                          (tock WORM-RIGHT)
                          (make-posn WIDTH HEIGHT)))

(define (tock-game gs)
  (if (eat? (game-state-worm gs)
            (game-state-food gs))
  (generate-food gs)
  (make-game-state (tock (game-state-worm gs))
                   (game-state-food gs))))

;; Worm Food -> Boolean
;; determines if worm eat food.
(check-expect (eat? WORM-RIGHT (make-posn CENTER-X CENTER-Y)) #true)
(check-expect (eat? WORM-RIGHT (make-posn WIDTH HEIGHT)) #false)

#;
(define (eat? worm food) #false)

(define (eat? worm food)
  (and (equal? (posn-x (first (worm-segments worm))) (posn-x food))
       (equal? (posn-y (first (worm-segments worm))) (posn-y food))))

;; GameState -> GameState
;; generate new food base and next snake
(check-expect (generate-food (make-game-state
                              WORM-RIGHT
                              (make-posn CENTER-X CENTER-Y)))
              
              (make-game-state
               (make-worm RIGHT (list HEAD BODY-1))
               (create-food (make-posn CENTER-X CENTER-Y)))) 

(define (generate-food gs)
  (make-game-state
   (create-next-worm (game-state-worm gs))
   (create-food (game-state-food gs))))

;; Worm -> Worm
;; create next snake
(check-expect (create-next-worm WORM-RIGHT) (make-worm RIGHT (list HEAD BODY-1)))

(define (create-next-worm w)
  (make-worm (worm-velocity w)
             (append (worm-segments w)
                     (list (make-posn (- (posn-x (first (worm-segments w))) (* 2 (image-width SEGMENT)))
                                      (posn-y (first (worm-segments w))))))))

;; Food -> Food
;; create new food.
;; !!!

(define (create-food f)
  (make-posn (* (random 30) (image-width SEGMENT))
             (* (random 24) (image-width SEGMENT))))

;; GameState KeyEvent -> GameState
;; handler worm input.
;; !!!

(define (game-key-handler gs ke)
  (make-game-state (key-handler (game-state-worm gs) ke)
                   (game-state-food gs)))

;; Application
;; ===========

;(main (make-game-state WORM-RIGHT FOOD))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants
;; =========

;; Polygon
(define SIZE-LENGTH 10)
(define SIZE-COUNT 8)
(define OUTLINE-MODE "solid")
(define SEGMENT-COLOR "red")
(define FOOD-COLOR "green")

;; Segment
(define SEGMENT (regular-polygon SIZE-LENGTH SIZE-COUNT OUTLINE-MODE SEGMENT-COLOR))

;; Food
(define FOOD-IMG (regular-polygon SIZE-LENGTH SIZE-COUNT OUTLINE-MODE FOOD-COLOR))

;; Display
(define WIDTH (* 30 (image-width SEGMENT)))
(define HEIGHT (* 24 (image-width SEGMENT)))
(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT "transparent"))

;; Worm
(define WORM-SPEED (/ (image-width SEGMENT) 4))

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
(define BODY-1 (make-posn (- (posn-x HEAD) (* 2 (image-width SEGMENT)))
                          (posn-y HEAD)))
(define BODY-2 (make-posn (- (posn-x HEAD) (* 4 (image-width SEGMENT)))
                          (posn-y HEAD)))
(define BODY-3 (make-posn (- (posn-x HEAD) (* 6 (image-width SEGMENT)))
                          (posn-y HEAD)))
(define BODY-4 (make-posn (- (posn-x HEAD) (* 8 (image-width SEGMENT)))
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


;; Food is (make-posn Natural[0, WIDTH] Natural[0, HEIGHT])
(define FOOD (make-posn (* (image-width SEGMENT) 3)
                        (* (image-width SEGMENT) 5)))

;; Represent a food in game.

(define-struct game-state
  [worm food])
;; GameState is (make-game-state Worm Food)
;; Represent a current game state.

#;
(define (fn-for-game-state gs)
  (... (fn-for-worm (game-state-worm gs))    ;Worm
       (fn-for-posn (game-state-food gs))))  ;Food / Posn

;; Functions
;; =========

;; GameState -> GameState
;; start the world with ...
;; !!!

(define (main w)
  (big-bang w
    (on-tick tock-game)
    (to-draw render-game)
    (on-key game-key-handler)
    (stop-when endgame? render-message)))

;; 

;; Worm -> Worm
;; procudes next worm's state.
(check-expect (tock (make-worm RIGHT
                               (list HEAD BODY-1 BODY-2)))
              (make-worm RIGHT
                         (list (make-posn (+ (posn-x HEAD) (* 2 WORM-SPEED))
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
              (list (make-posn (+ (posn-x HEAD) (* 2 WORM-SPEED))
                               (posn-y HEAD))))
(check-expect (next-segments RIGHT (list HEAD BODY-1 BODY-2))
              (list (make-posn (+ (posn-x HEAD) (* 2 WORM-SPEED))
                               (posn-y HEAD))
                    HEAD
                    BODY-1))
#;
(define (next-segments velocity segments) empty)

(define (next-segments velocity segments)
  (cons (make-posn (+ (posn-x (first segments)) (* (posn-x velocity) (* 2 WORM-SPEED)))
                   (+ (posn-y (first segments)) (* (posn-y velocity) (* 2 WORM-SPEED))))
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
(check-expect (render-worm WORM-LEFT)
              (place-image SEGMENT CENTER-X CENTER-Y MTS))

#;
(define (render-worm w) MTS)

(define (render-worm w)
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


;; GameState -> Boolean.
;; determinse if end game.
(define (endgame? gs)
  (or (worm-hit-wall? (game-state-worm gs))
      (worm-hit-self? (game-state-worm gs))))


;; Worm -> Boolean
;; determinse if end game.

#;
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

;; Food -> Image
;; render food.
(check-expect (render-food FOOD) (place-image FOOD-IMG (posn-x FOOD) (posn-y FOOD) MTS))

#;
(define (render-food food) MTS)

(define (render-food food)
  (place-image FOOD-IMG (posn-x food) (posn-y food) MTS))


;; GameState -> Image
;; if worm eat food, generate new food otherwise move worm.
(check-expect (render-game (make-game-state WORM-RIGHT FOOD))
              (overlay (render-worm WORM-RIGHT)
                       (render-food FOOD)))

#;
(define (render-game gs) MTS)

(define (render-game gs)
  (overlay (render-worm (game-state-worm gs))
           (render-food (game-state-food gs))))

;; GameState -> GameState
;; render worm and food.
(check-expect (tock-game (make-game-state
                          WORM-RIGHT
                          (make-posn CENTER-X CENTER-Y)))
              (generate-food (make-game-state
                          WORM-RIGHT
                          (make-posn CENTER-X CENTER-Y))))
(check-expect (tock-game (make-game-state
                          WORM-RIGHT
                          (make-posn WIDTH HEIGHT)))
              (make-game-state
                          (tock WORM-RIGHT)
                          (make-posn WIDTH HEIGHT)))

(define (tock-game gs)
  (if (eat? (game-state-worm gs)
            (game-state-food gs))
  (generate-food gs)
  (make-game-state (tock (game-state-worm gs))
                   (game-state-food gs))))

;; Worm Food -> Boolean
;; determines if worm eat food.
(check-expect (eat? WORM-RIGHT (make-posn CENTER-X CENTER-Y)) #true)
(check-expect (eat? WORM-RIGHT (make-posn WIDTH HEIGHT)) #false)

#;
(define (eat? worm food) #false)

(define (eat? worm food)
  (and (equal? (posn-x (first (worm-segments worm))) (posn-x food))
       (equal? (posn-y (first (worm-segments worm))) (posn-y food))))

;; GameState -> GameState
;; generate new food base and next snake
(check-expect (generate-food (make-game-state
                              WORM-RIGHT
                              (make-posn CENTER-X CENTER-Y)))
              
              (make-game-state
               (make-worm RIGHT (list HEAD BODY-1))
               (create-food (make-posn CENTER-X CENTER-Y)))) 

(define (generate-food gs)
  (make-game-state
   (create-next-worm (game-state-worm gs))
   (create-food (game-state-food gs))))

;; Worm -> Worm
;; create next snake
(check-expect (create-next-worm WORM-RIGHT) (make-worm RIGHT (list HEAD BODY-1)))

(define (create-next-worm w)
  (make-worm (worm-velocity w)
             (append (worm-segments w)
                     (list (make-posn (- (posn-x (first (worm-segments w))) (* 2 (image-width SEGMENT)))
                                      (posn-y (first (worm-segments w))))))))

;; Food -> Food
;; create new food.
;; !!!

(define (create-food f)
  (make-posn (* (random 30) (image-width SEGMENT))
             (* (random 24) (image-width SEGMENT))))

;; GameState KeyEvent -> GameState
;; handler worm input.
;; !!!

(define (game-key-handler gs ke)
  (make-game-state (key-handler (game-state-worm gs) ke)
                   (game-state-food gs)))

;; Application
;; ===========

;(main (make-game-state WORM-RIGHT FOOD))
