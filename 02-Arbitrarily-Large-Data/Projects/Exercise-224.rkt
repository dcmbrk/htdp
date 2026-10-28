;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-224) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; =========
;; Constants

(define INVADER (bitmap/file "invader.png"))
(define SPACE (bitmap/file "space.png"))
(define MISSIBLE (bitmap/file "missible.png"))

(define SIZE 10)
(define SCENE-SIZE (* SIZE SIZE))
(define WIDTH 10)
(define HEIGHT 10)
(define CENTER-X (/ WIDTH 2))
(define SHIFT (/ SIZE 2))
(define BACKGROUND-COLOR "white")
(define SCENE (empty-scene SCENE-SIZE SCENE-SIZE BACKGROUND-COLOR))
(define RIGHT (make-posn 1 0))
(define LEFT (make-posn -1 0))

;; ===============
;; Data definition
(define-struct game [space invaders missibles])
(define-struct invader [position velocity])

;; An Game is structure: (make-game Posn (listof Invader) (listof Posn)) 

;; An Invader is structure: (make-invader Posn Posn)

;; (listof Invader) is one of:
;; - empty
;; - (cons Invader (listof Invader))

;; A Missible is Posn

;; (listof Missible) is (listof Posn):

;; A Space is Posn

;; represents a space-invader game
;; invaders moving constanstly with velocity, when hit WIDTH boundary them move down and change direction, if hit HEIGHT boundary or player then game over.
;; space can move left to right, shoot missible.
;; missible move up constantly, if hit invader then kill self and invader, if hit HEIGHT boundary kill itself.

;; =========
;; Templates

#;
(define (fn-for-game g)
  (... (fn-for-posn (game-space g))   ;Posn
       (game-invaders g)              ;(listof Invader)
       (game-missibles g)))           ;(listof Posn)

#;
(define (fn-for-posns lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-posn (first lop))
              (fn-for-posns (rest lop)))]))

#;
(define (fn-for-posn p)
  (... (posn-x p)
       (posn-y p)))

#;
(define (fn-for-invaders loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-invaders (rest loi)))]))

#;
(define (fn-for-invader i)
  (... (invader-position i)
       (invader-velocity i)))

;; =============
;; Examples data

(define SPACE-START
  (make-posn (- CENTER-X 1)
             (- HEIGHT 1)))

(define INVADER-1 (make-invader (make-posn 2 0) RIGHT))
(define INVADER-2 (make-invader (make-posn 3 0) RIGHT))
(define INVADER-3 (make-invader (make-posn 4 0) RIGHT))
(define INVADER-4 (make-invader (make-posn 5 0) RIGHT))
(define INVADER-5 (make-invader (make-posn 6 0) RIGHT))
(define INVADER-6 (make-invader (make-posn 7 0) RIGHT))

(define INVADERS (list INVADER-1 INVADER-2 INVADER-3 INVADER-4 INVADER-5 INVADER-6))

(define MISSIBLE-START (make-posn (- CENTER-X 1)
                                  (- HEIGHT 2)))
(define MISSIBLES (list MISSIBLE-START))

(define GAME-0 (make-game SPACE-START INVADERS empty))
(define GAME-1 (make-game SPACE-START INVADERS MISSIBLES))

;; =========
;; Functions

;; Game -> Game
;; starts with ...

(define (game-main g rate)
  (big-bang g
    [on-tick game-tock rate]
    [to-draw game-render]
    [stop-when game-end?]
    [on-key game-controller]))

;; Game -> Game
;; update next state: game.
(check-expect
 (game-tock
  (make-game SPACE-START
             (list (make-invader (make-posn 2 0) RIGHT)
                   (make-invader (make-posn 3 0) RIGHT)
                   (make-invader (make-posn 4 0) RIGHT)
                   (make-invader (make-posn 5 0) RIGHT)
                   (make-invader (make-posn 6 0) RIGHT)
                   (make-invader (make-posn 7 0) RIGHT))
             empty))
 (make-game SPACE-START
            (list (make-invader (make-posn 3 0) RIGHT)
                  (make-invader (make-posn 4 0) RIGHT)
                  (make-invader (make-posn 5 0) RIGHT)
                  (make-invader (make-posn 6 0) RIGHT)
                  (make-invader (make-posn 7 0) RIGHT)
                  (make-invader (make-posn 8 0) RIGHT))
            empty))
(check-expect
 (game-tock
  (make-game SPACE-START
             (list (make-invader (make-posn (- WIDTH 1) 0) RIGHT));
             empty))
 (make-game SPACE-START
            (list (make-invader (make-posn (- WIDTH 1) 1) LEFT));
            empty))
(check-expect
 (game-tock
  (make-game SPACE-START
             (list (make-invader (make-posn (- WIDTH 1) 1) LEFT));
             empty))
 (make-game SPACE-START
            (list (make-invader (make-posn (- WIDTH 2) 1) LEFT));
            empty))
(check-expect
 (game-tock
  (make-game SPACE-START
             (list (make-invader (make-posn 0 0) LEFT));
             empty))
 (make-game SPACE-START
            (list (make-invader (make-posn 0 1) RIGHT));
            empty))
(check-expect
 (game-tock
  (make-game SPACE-START
             (list (make-invader (make-posn 0 1) RIGHT));
             empty))
 (make-game SPACE-START
            (list (make-invader (make-posn 1 1) RIGHT));
            empty))


(define (game-tock g)
  (local [(define next-invaders (invaders-tock (game-invaders g)))
          (define next-missibles (missibles-tock (game-missibles g)))]
    (make-game (game-space g)
               (filter-invaders next-invaders next-missibles)
               (fitler-missibles
                (set-posns
                 next-missibles
                 (invaders->posns next-invaders)))))) 

;; (listof Invader) -> (listof Invader)
;; update states: list of invader.
(check-expect (invaders-tock
               (list (make-invader (make-posn 2 0) RIGHT)
                     (make-invader (make-posn 3 0) RIGHT)
                     (make-invader (make-posn 4 0) RIGHT)
                     (make-invader (make-posn 5 0) RIGHT)
                     (make-invader (make-posn 6 0) RIGHT)
                     (make-invader (make-posn 7 0) RIGHT)))
              (list (make-invader (make-posn 3 0) RIGHT)
                    (make-invader (make-posn 4 0) RIGHT)
                    (make-invader (make-posn 5 0) RIGHT)
                    (make-invader (make-posn 6 0) RIGHT)
                    (make-invader (make-posn 7 0) RIGHT)
                    (make-invader (make-posn 8 0) RIGHT)))
(check-expect (invaders-tock
               (list (make-invader (make-posn (- WIDTH 1) 0) RIGHT)));
              (list (make-invader (make-posn (- WIDTH 1) 1) LEFT)));
(check-expect (invaders-tock
               (list (make-invader (make-posn (- WIDTH 1) 1) LEFT)));
              (list (make-invader (make-posn (- WIDTH 2) 1) LEFT)));
(check-expect (invaders-tock
               (list (make-invader (make-posn 0 0) LEFT)));
              (list (make-invader (make-posn 0 1) RIGHT)));
(check-expect (invaders-tock
               (list (make-invader (make-posn 0 1) RIGHT)));
              (list (make-invader (make-posn 1 1) RIGHT)))

#;
(define (invaders-tock loi) loi)

(define (invaders-tock loi)
  (cond [(empty? loi) empty]
        [else
         (cons (invader-tock (first loi))
               (invaders-tock (rest loi)))]))

;; Invader -> Invader
;; update state: invader.
(check-expect (invader-tock (make-invader (make-posn (- WIDTH 1) 0) RIGHT))
              (make-invader (make-posn (- WIDTH 1) 1) LEFT))
(check-expect (invader-tock (make-invader (make-posn (- WIDTH 1) 1) LEFT))
              (make-invader (make-posn (- WIDTH 2) 1) LEFT))
(check-expect (invader-tock (make-invader (make-posn 0 0) LEFT))
              (make-invader (make-posn 0 1) RIGHT))
(check-expect (invader-tock (make-invader (make-posn 0 1) RIGHT))
              (make-invader (make-posn 1 1) RIGHT))

(define (invader-tock i)
  (if (out-x-boundary? (invader-position (invader-move i)))
   (invader-change i)
   (invader-move i)))

;; Posn -> Boolean
;; determines position out of x boundary.
(check-expect (out-x-boundary? (make-posn 0 0)) #false)
(check-expect (out-x-boundary? (make-posn -1 0)) #true)
(check-expect (out-x-boundary? (make-posn WIDTH 0)) #true)

(define (out-x-boundary? p)
  (or (>= (posn-x p) WIDTH)
       (< (posn-x p) 0)))

;; Invader -> Invader
;; update invader move to velocity.
(check-expect (invader-move (make-invader (make-posn 0 0) RIGHT))
              (make-invader (make-posn 1 0) RIGHT))

#;
(define (invader-move i) i)

(define (invader-move i)
  (make-invader
   (make-posn
    (+ (posn-x (invader-position i)) (posn-x (invader-velocity i)))
    (posn-y (invader-position i)))
   (invader-velocity i)))

;; Invader -> Invader
;; update invader to change direction and move to next row.
(check-expect (invader-change
               (make-invader (make-posn (- WIDTH 1) 0) RIGHT))
              (make-invader (make-posn (- WIDTH 1) 1) LEFT))
(check-expect (invader-change
               (make-invader (make-posn 0 0) LEFT))
              (make-invader (make-posn 0 1) RIGHT))

#;
(define (invader-change i) i)

(define (invader-change i)
  (make-invader
   (make-posn
    (posn-x (invader-position i))
    (+ (posn-y (invader-position i)) 1))
   (make-posn
    (* (posn-x (invader-velocity i)) -1)
    (posn-y (invader-velocity i)))))


;; (listof Missible) -> (listof Missible)
;; update states: list of missible.
(check-expect
 (missibles-tock
  (list (make-posn (- CENTER-X 1) (- HEIGHT 2))))
 (list (make-posn (- CENTER-X 1) (- HEIGHT 3))))

#;
(define (missibles-tock lom) lom)

(define (missibles-tock lop)
  (cond [(empty? lop) empty]
        [else
         (cons (missible-tock (first lop))
               (missibles-tock (rest lop)))]))

;; Missible -> Missible
;; update states: missible.
(check-expect (missible-tock (make-posn (- CENTER-X 1) (- HEIGHT 2)))
              (make-posn (- CENTER-X 1) (- HEIGHT 3)))

(define (missible-tock p)
  (make-posn (posn-x p)
             (- (posn-y p) 1)))

;; Game -> Image
;; render invaders, space, missibles to scene.
(check-expect (game-render GAME-0)
              (place-images
               (list SPACE INVADER INVADER INVADER INVADER INVADER INVADER)
               (list
                (make-posn (+ (* SIZE (- CENTER-X 1)) SHIFT) (+ (* SIZE (- HEIGHT 1)) SHIFT))
                (make-posn (+ (* SIZE 2) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 3) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 4) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 5) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 6) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 7) SHIFT) (+ SHIFT (* SIZE 0))))
               SCENE))
(check-expect (game-render GAME-1)
              (place-images
               (list SPACE INVADER INVADER INVADER INVADER INVADER INVADER MISSIBLE)
               (list
                (make-posn (+ (* SIZE (- CENTER-X 1)) SHIFT) (+ (* SIZE (- HEIGHT 1)) SHIFT))
                (make-posn (+ (* SIZE 2) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 3) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 4) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 5) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 6) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE 7) SHIFT) (+ SHIFT (* SIZE 0)))
                (make-posn (+ (* SIZE (- CENTER-X 1)) SHIFT) (+ (* SIZE (- HEIGHT 2)) SHIFT)))
               SCENE))

#;
(define (game-render g) SCENE)

(define (game-render g)
  (place-images (game->images g)
                (shift-posns (game->posns g))
                SCENE))

;; Game -> (listof Image)
;; creates a list of image: space, invaders, missibles.
(check-expect (game->images GAME-0)
              (list SPACE INVADER INVADER INVADER INVADER INVADER INVADER))
(check-expect (game->images GAME-1)
              (list SPACE INVADER INVADER INVADER INVADER INVADER INVADER MISSIBLE))

#;
(define (game->images g) empty)

(define (game->images g)
  (append
   (cons SPACE
         (invaders->images (game-invaders g)))
   (missibles->images (game-missibles g))))

;; (listof Invader) -> (listof Image)
;; creates a list of images from list of invader.
(check-expect (invaders->images (game-invaders GAME-0))
              (list INVADER INVADER INVADER INVADER INVADER INVADER))

#;
(define (invaders->images loi) empty)

(define (invaders->images loi)
  (make-list (length loi) INVADER))

;; (listof Missible) -> (listof Image)
;; creates a list of images from list of missible.
(check-expect (missibles->images (game-missibles GAME-0)) empty)
(check-expect (missibles->images (game-missibles GAME-1)) (list MISSIBLE))

#;
(define (missibles->images lom) empty)

(define (missibles->images lom)
  (make-list (length lom) MISSIBLE))

;; Game -> (listof Posn)
;; creates a list of posn: space, invaders, missibles.
(check-expect (game->posns GAME-0)
              (list (game-space GAME-0)
                    (invader-position INVADER-1)
                    (invader-position INVADER-2)
                    (invader-position INVADER-3)
                    (invader-position INVADER-4)
                    (invader-position INVADER-5)
                    (invader-position INVADER-6)))
(check-expect (game->posns GAME-1)
              (list (game-space GAME-1)
                    (invader-position INVADER-1)
                    (invader-position INVADER-2)
                    (invader-position INVADER-3)
                    (invader-position INVADER-4)
                    (invader-position INVADER-5)
                    (invader-position INVADER-6)
                    (first (game-missibles GAME-1))))

#;
(define (game->posns g) empty)

(define (game->posns g)
  (append
   (cons (game-space g)
         (invaders->posns (game-invaders g)))
   (game-missibles g)))

;; (listof Invader) -> (listof Posn)
;; creates list of posn from list of invader.
(check-expect (invaders->posns (game-invaders GAME-0))
              (list (invader-position INVADER-1)
                    (invader-position INVADER-2)
                    (invader-position INVADER-3)
                    (invader-position INVADER-4)
                    (invader-position INVADER-5)
                    (invader-position INVADER-6)))
#;
(define (invaders->posns loi) empty)

(define (invaders->posns loi)
  (cond [(empty? loi) empty]
        [else
         (cons (invader-position (first loi))
               (invaders->posns (rest loi)))]))

;; (listof Posn) -> (listof Posn)
;; shift every posn.
(check-expect (shift-posns (game->posns GAME-0))
              (list
               (make-posn (+ (* SIZE (- CENTER-X 1)) SHIFT) (+ (* SIZE (- HEIGHT 1)) SHIFT))
               (make-posn (+ (* SIZE 2) SHIFT) (+ SHIFT (* SIZE 0)))
               (make-posn (+ (* SIZE 3) SHIFT) (+ SHIFT (* SIZE 0)))
               (make-posn (+ (* SIZE 4) SHIFT) (+ SHIFT (* SIZE 0)))
               (make-posn (+ (* SIZE 5) SHIFT) (+ SHIFT (* SIZE 0)))
               (make-posn (+ (* SIZE 6) SHIFT) (+ SHIFT (* SIZE 0)))
               (make-posn (+ (* SIZE 7) SHIFT) (+ SHIFT (* SIZE 0)))))

#;
(define (shift-posns lop) lop)

(define (shift-posns lop)
  (cond [(empty? lop) empty]
        [else
         (cons (shift-posn (first lop))
               (shift-posns (rest lop)))]))

;; Posn -> Posn
;; shift posn.
(check-expect (shift-posn (make-posn 2 0))
              (make-posn (+ (* SIZE 2) SHIFT) (+ SHIFT (* SIZE 0))))

(define (shift-posn p)
  (make-posn (+ (* SIZE (posn-x p)) SHIFT)
             (+ SHIFT (* SIZE (posn-y p)))))

;; Game -> Boolean
;; determines if game is end.
(check-expect (game-end?
               (make-game SPACE-START
                          (list (make-invader
                                 (make-posn (- CENTER-X 1)
                                            (- HEIGHT 1))
                                 RIGHT))
                          empty))
              #true)
 (check-expect (game-end?
               (make-game SPACE-START
                          (list (make-invader
                                 (make-posn 0 HEIGHT)
                                 RIGHT))
                          empty))
              #true)
(check-expect (game-end? GAME-0) #false)
(check-expect (game-end? GAME-1) #false)

(define (game-end? g)
  (or
   (invaders-out-y-boundary? (game-invaders g))
   (invader-hit-space? (game-invaders g) (game-space g))
   (out-of-invaders? (game-invaders g))))

;; (listof Invader) -> Boolean
;; determines if out of invaders.

(define (out-of-invaders? loi)
  (= (length loi) 0))

;; (listof Invader) -> Boolean
;; determines if has invader out of bound.
(check-expect (invaders-out-y-boundary? INVADERS) #false)
(check-expect (invaders-out-y-boundary? (list (make-invader (make-posn 0 HEIGHT) RIGHT))) #true)

#;
(define (invaders-out-y-boundary? loi) #false)

(define (invaders-out-y-boundary? loi)
  (cond [(empty? loi) #false]
        [else
         (or (invader-out-y-boundary? (first loi))
             (invaders-out-y-boundary? (rest loi)))]))

;; Invader -> Boolean
;; determinse if invaders out of y boundary.
(check-expect (invader-out-y-boundary? INVADER-1) #false)
(check-expect (invader-out-y-boundary? (make-invader (make-posn 0 HEIGHT) RIGHT)) #true)

(define (invader-out-y-boundary? i)
  (out-y-boundary? (invader-position i)))

;; Posn -> Boolean
;; determinse if posn out of y boundary.
(check-expect (out-y-boundary? (make-posn 0 0)) #false)
(check-expect (out-y-boundary? (make-posn 0 HEIGHT)) #true)

#;
(define (out-y-boundary? p) #false)

(define (out-y-boundary? p)
  (or (>= (posn-y p) HEIGHT)
      (< (posn-y p) 0)))

;; (listof Invader) Space -> Boolean
;; determines if has invader hit space.
(check-expect (invader-hit-space? INVADERS SPACE) #false)
(check-expect (invader-hit-space? (list INVADER-1 INVADER-2 (make-invader (make-posn (- CENTER-X 1) (- HEIGHT 1)) RIGHT)) SPACE-START) #true)

(define (invader-hit-space? loi p)
  (member? p (invaders->posns loi)))

;; Game KeyEvent -> Game
;; handler input: game.
(check-expect (game-controller GAME-0 " ") GAME-1)
(check-expect (game-controller GAME-0 "left")
              (make-game (make-posn 3 9) INVADERS empty))
(check-expect (game-controller GAME-0 "right")
              (make-game (make-posn 5 9) INVADERS empty))

#;
(define (game-controller g ke) g)

(define (game-controller g ke)
  (cond [(key=? ke "left")
         (make-game
          (move-space (game-space g) ke)
          (game-invaders g)
          (game-missibles g))]
        [(key=? ke "right")
         (make-game
          (move-space (game-space g) ke)
          (game-invaders g)
          (game-missibles g))]
        [(key=? ke " ")
         (make-game
          (game-space g)
          (game-invaders g)
          (spawn-missible (game-space g) (game-missibles g)))]
        [else g]))

;; Space Ke -> Space
;; move space base on key event.
(check-expect (move-space SPACE-START "left") (make-posn 3 9))
(check-expect (move-space SPACE-START "right") (make-posn 5 9))

#;
(define (move-space s ke) s)

(define (move-space s ke)
  (cond [(key=? ke "left")
         (make-posn
          (- (posn-x s) 1)
          (posn-y s))]
        [(key=? ke "right")
         (make-posn
          (+ (posn-x s) 1)
          (posn-y s))]
        [else s]))

;; (listof Missible) Space -> (listof Missible)
;; spawn new missible.
(check-expect (spawn-missible SPACE-START (list (make-posn 2 2)))
              (list MISSIBLE-START (make-posn 2 2)))

(define (spawn-missible s lom)
  (cons
   (make-posn
    (posn-x s)
    (- (posn-y s) 1))
   lom))

;; (listof Posn) (listof Posn) -> (listof Posn)
;; remove doubles positon of 1st from 2nd.
(check-expect (set-posns empty empty) empty)
(check-expect (set-posns
               (list (make-posn 0 0))
               empty)
              (list (make-posn 0 0)))
(check-expect (set-posns
               (list (make-posn 0 0) (make-posn 1 1))
               (list (make-posn 0 0)))
              (list (make-posn 1 1)))
(check-expect (set-posns
               (list (make-posn 2 2) (make-posn 1 1))
               (list (make-posn 0 0)))
              (list (make-posn 2 2) (make-posn 1 1)))

#;
(define (set-posns lop1 lop2) lop1)
 
(define (set-posns lop1 lop2)
  (cond [(empty? lop1) empty]
        [(empty? lop2) lop1]
        [else
         (if (member? (first lop1) lop2)
             (set-posns (rest lop1) lop2)
             (cons (first lop1)
                   (set-posns (rest lop1) lop2)))]))

;; (listof Invader) (listof Missible) -> (listof Invader)
;; filter invaders.
(check-expect (filter-invaders (list INVADER-1) (list (invader-position INVADER-1))) empty)
(check-expect (filter-invaders (list INVADER-1) (list MISSIBLE-START)) (list INVADER-1))

#;
(define (filter-invaders loi lop) loi) 

(define (filter-invaders loi lop)
  (cond [(empty? loi) empty]
        [else
         (if (member? (invader-position (first loi)) lop)
             (filter-invaders (rest loi) lop)
             (cons (first loi) (filter-invaders (rest loi) lop)))]))

;; (listof Missible) -> (listof Missible)
;; filter out of y bound missible

(define (fitler-missibles lom)
  (cond [(empty? lom) empty]
        [else
         (if (out-y-boundary? (first lom))
             (fitler-missibles (rest lom))
             (cons (first lom) (fitler-missibles (rest lom))))]))

;; Application
;; ===========

(game-main GAME-0 1)
 