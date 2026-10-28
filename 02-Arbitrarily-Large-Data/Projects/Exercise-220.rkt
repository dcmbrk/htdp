;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constanst
;; =========
(define WIDTH 10) ;width of blocks, horizontally
(define HEIGHT WIDTH)
(define CENTER-X (/ WIDTH 2))
(define SIZE 10)  ;blocks are squares
(define SHIFT (/ SIZE 2))
(define SCENE-SIZE (* WIDTH SIZE))
(define SCENE (empty-scene SCENE-SIZE SCENE-SIZE "white"))

(define BLOCK     ;blocks is red square with outline
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))

;; Data definitions
;; ================
(define-struct tetris [block landscrape])
(define-struct block [x y])

;; A Tetris is a structure: (make-tetris Block Landscrape)

;; A Landscape is one of:
;; - empty
;; - (cons Block Landscrape)

;; A Block is a structure: (make-block Number Number)

;; Represents
;; (make-block x y) is a block whose left conner is (* x SIZE) pixels from the left and (* y SIZE) pixels from the top.
;; (make-tetris BLOCK-0 (list BLOCK-1 BLOCK-2 ...)) means BLOCK-0 is the dropping block, while BLOCK-1, BLOCK-2, ... are resting. 

#;
(define (fn-for-tetris t)
  (... (fn-for-block (tetris-block t))
       (fn-for-landscrape (tetris-landscrape t))))

#;
(define (fn-for-block b)
  (... (block-x b)
       (block-y b)))

#;
(define (fn-for-landscrape l)
  (cond [(empty? l) (...)]
        [else
         (... (fn-for-block (first l))
              (fn-for-landscrape (rest l)))]))

(define BLOCK-START (make-block (- CENTER-X 1) 0))
(define BLOCK-LAND-CENTER (make-block (- CENTER-X 1) (- HEIGHT 1)))
(define BLOCK-ON-BLOCK-CENTER (make-block (- CENTER-X 1) (- HEIGHT 2)))
(define BLOCK-ON-RIGHT-CONNER (make-block (- WIDTH 1) (- HEIGHT 1)))

(define TETRIS-0 (make-tetris BLOCK-START empty))
(define TETRIS-1 (make-tetris BLOCK-START (list BLOCK-LAND-CENTER)))
(define TETRIS-2 (make-tetris BLOCK-START (list BLOCK-ON-BLOCK-CENTER BLOCK-LAND-CENTER)))
(define TETRIS-3 (make-tetris BLOCK-START (list BLOCK-ON-RIGHT-CONNER BLOCK-ON-BLOCK-CENTER BLOCK-LAND-CENTER)))

;; Functions
;; =========
;; Tetris -> Image
;; render tetris states to SCENE.
(check-expect (tetris-render TETRIS-0)
              (place-images (list BLOCK)
                            (list
                             (make-posn (+ (* (block-x BLOCK-START) SIZE) SHIFT)
                                        (+ (* (block-y BLOCK-START) SIZE) SHIFT)))
                            SCENE))
(check-expect (tetris-render TETRIS-1)
              (place-images (list BLOCK BLOCK)
                            (list
                             (make-posn (+ (* (block-x BLOCK-START) SIZE) SHIFT)
                                        (+ (* (block-y BLOCK-START) SIZE) SHIFT))
                             (make-posn (+ (* (block-x BLOCK-LAND-CENTER) SIZE) SHIFT)
                                        (+ (* (block-y BLOCK-LAND-CENTER) SIZE) SHIFT)))
                            SCENE))
(check-expect (tetris-render TETRIS-2)
              (place-images (list BLOCK BLOCK BLOCK)
                            (list
                             (make-posn (+ (* (block-x BLOCK-START) SIZE) SHIFT)
                                        (+ (* (block-y BLOCK-START) SIZE) SHIFT))
                             (make-posn (+ (* (block-x BLOCK-ON-BLOCK-CENTER) SIZE) SHIFT)
                                        (+ (* (block-y BLOCK-ON-BLOCK-CENTER) SIZE) SHIFT))
                             (make-posn (+ (* (block-x BLOCK-LAND-CENTER) SIZE) SHIFT)
                                        (+ (* (block-y BLOCK-LAND-CENTER) SIZE) SHIFT)))
                            SCENE))

#;
(define (tetris-render t) SCENE)

(define (tetris-render t)
  (place-images
   (tetris->blocks t) ;Blocks
   (tetris->posns t)  ;Posns
   ;(fn-for-block (tetris-block t))
   ;(fn-for-landscrape (tetris-landscrape t))
   SCENE))

;; Tetris -> Block
;; creates list of block images for dropping block and landscrape blocks.
(check-expect (tetris->blocks TETRIS-0) (list BLOCK))
(check-expect (tetris->blocks TETRIS-1) (list BLOCK BLOCK))
(check-expect (tetris->blocks TETRIS-2) (list BLOCK BLOCK BLOCK))

#;
(define (tetris->blocks t) empty)

(define (tetris->blocks t)
  (make-list
   (+ 1 (length (tetris-landscrape t)))
   BLOCK))

;; Tetris -> Posns
;; creates positions of dropping block and landscrape block
(check-expect (tetris->posns TETRIS-0)
              (list
               (make-posn (+ (* (block-x BLOCK-START) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-START) SIZE) SHIFT))))
(check-expect (tetris->posns TETRIS-3)
              (list
               (make-posn (+ (* (block-x BLOCK-START) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-START) SIZE) SHIFT))
               (make-posn (+ (* (block-x BLOCK-ON-RIGHT-CONNER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-ON-RIGHT-CONNER) SIZE) SHIFT))
               (make-posn (+ (* (block-x BLOCK-ON-BLOCK-CENTER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-ON-BLOCK-CENTER) SIZE) SHIFT))
               (make-posn (+ (* (block-x BLOCK-LAND-CENTER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-LAND-CENTER) SIZE) SHIFT))))

#;
(define (tetris->posns t) empty)

(define (tetris->posns t)
  (cons (block->posn (tetris-block t))
        (blocks->posns (tetris-landscrape t))))

;; Blocks -> Posns
;; create position of blocks.
(check-expect (blocks->posns (list BLOCK-LAND-CENTER))
              (list
               (make-posn (+ (* (block-x BLOCK-LAND-CENTER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-LAND-CENTER) SIZE) SHIFT))))
(check-expect (blocks->posns (list BLOCK-ON-RIGHT-CONNER
                                   BLOCK-ON-BLOCK-CENTER
                                   BLOCK-LAND-CENTER))
              (list
               (make-posn (+ (* (block-x BLOCK-ON-RIGHT-CONNER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-ON-RIGHT-CONNER) SIZE) SHIFT))
               (make-posn (+ (* (block-x BLOCK-ON-BLOCK-CENTER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-ON-BLOCK-CENTER) SIZE) SHIFT))
               (make-posn (+ (* (block-x BLOCK-LAND-CENTER) SIZE) SHIFT)
                          (+ (* (block-y BLOCK-LAND-CENTER) SIZE) SHIFT))))

#;
(define (blocks->posns blocks) empty)

(define (blocks->posns l)
  (cond [(empty? l) empty]
        [else
         (cons (block->posn (first l))
               (blocks->posns (rest l)))]))


;; Block -> Posn
;; create position for block.
(check-expect (block->posn BLOCK-LAND-CENTER)
              (make-posn (+ (* (block-x BLOCK-LAND-CENTER) SIZE) SHIFT)
                         (+ (* (block-y BLOCK-LAND-CENTER) SIZE) SHIFT)))

(define (block->posn b)
  (make-posn (+ (* (block-x b) SIZE) SHIFT)
             (+ (* (block-y b) SIZE) SHIFT)))