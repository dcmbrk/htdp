;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-208) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

(define-struct date
  [year month day hour minute second])
;; Date is (make-date Number Number Number Number Number Number)
;; Represents an instance of date

#;
(define (fn-for-date date)
  (... (date-year date)
       (date-month date)
       (date-day date)
       (date-hour date)
       (date-minute date)
       (date-second date)))

;; Association is one list of 2 items:
;; - (list String BSDN)
;; Represents an instance of tuple contain key and value

#;
(define (fn-for-association assoc)
  (... (first assoc)     ;String
       (second assoc)))  ;BSDN

;; BSDN is one of:
;; - Boolean
;; - Number
;; - String
;; - Date
;; Reperesents a value of an Association.

(define (fn-for-bsdn bsdn)
  (cond [(boolean? bsdn) (...)]
        [(number? bsdn) (...)]
        [(string? bsdn) (...)]
        [(date? bsdn) (...)]))

;; Associations is one of:
;; - empty
;; - (cons Association Associations)
;; Represent a list of Association.

#;
(define (fn-for-associations assocs)
  (cond [(empty? assocs) (...)]
        [else
         (... (fn-for-association (first assocs))
              (fn-for-associations (rest assocs)))]))

;; Track is Associations
;; Represents a track in itunes.

;; Tracks is (listof Track)
;; Represents a list of track.

#;
(define (fn-for-tracks tracks)
  (cond [(empty? tracks) (...)]
        [else
         (... (fn-for-associations (first tracks))
              (fn-for-tracks (rest tracks)))]))

;; Data
;; ====

;; Association
(define NAME (list "Name" "Wild Child"))
(define ARTIST (list "Artist" "Enya"))
(define ALBUM (list "Album" "A Day Without Rain"))
(define TIME (list "Total Time" 227996))
(define TRACK# (list "Track Number" 2))
(define PLAY# (list "Play Count" 20))

;; Track
(define TRACK-1
  (list (list "Name" "Wild Child")
        (list "Artist" "Enya")
        (list "Album" "A Day Without Rain")
        (list "Total Time" 227996)
        (list "Track Number" 2)
        (list "Play Count" 20)))
(define TRACK-2
  (list (list "Name" "Only Time")
        (list "Artist" "Enya")
        (list "Album" "A Day Without Rain")
        (list "Total Time" 218000)
        (list "Track Number" 3)
        (list "Play Count" 15)))


;; String Associations Any -> Associaition or Any
;; produces value of first item that equal to key otherwise produces default.
(check-expect (find-association "Name" TRACK-1 "Not found") (list "Name" "Wild Child"))
(check-expect (find-association "Total time" TRACK-1 "Not found") "Not found")

#;
(define (find-association key assocs default) default)

(define (find-association key assocs default)
  (cond [(empty? assocs) default]
        [else
         (if (string=? key (first (first assocs)))
             (first assocs)
             (find-association key (rest assocs) default))]))

;; Tracks -> ListOfString
;; produces a list of field that is has boolean value from given tracks.
(check-expect (boolean-attributes--tracks (list (list (list "a" 1) (list "b" #true) (list "c" #false) (list "d" 4))
                                              (list (list "a" 1) (list "b" #true) (list "c" 3) (list "d" 4))))
              (list "c" "b"))
 
(check-expect (boolean-attributes--tracks empty) empty)

(define (boolean-attributes--tracks tracks)
  (create-set (boolean-attributes--list tracks)))

;; (listof Associations) -> ListOfString
;; produces a list of field that is has boolean value from given list of associations.
(check-expect (boolean-attributes--list (list (list (list "a" 1) (list "b" #true) (list "c" #false) (list "d" 4))
                                              (list (list "a" 1) (list "b" #true) (list "c" 3) (list "d" 4))))
              (list "b" "c" "b"))
(check-expect (boolean-attributes--list empty) empty)

#;
(define (boolean-attributes--list lassocs) empty) 

(define (boolean-attributes--list lassocs)
  (cond [(empty? lassocs) empty]
        [else
         (append (boolean-attributes--item (first lassocs))
                 (boolean-attributes--list (rest lassocs)))]))


;; Associations -> ListOfString
;; produces a list of field that is has boolean value.
(check-expect (boolean-attributes--item (list (list "a" 1) (list "b" #true) (list "c" #false) (list "d" 4)))
              (list "b" "c"))
(check-expect (boolean-attributes--item (list (list "a" 1) (list "b" 2) (list "c" 3) (list "d" 4)))
              empty)

#;
(define (boolean-attributes--item assocs) empty)

(define (boolean-attributes--item assocs)
  (cond [(empty? assocs) empty]
        [else
         (if (boolean? (second (first assocs)))
             (cons (first (first assocs))
                   (boolean-attributes--item (rest assocs)))
             (boolean-attributes--item (rest assocs)))]))

;; ListOfString -> ListOfString
;; produces unique set of string.
(check-expect (create-set empty) empty)
(check-expect (create-set (list "a" "b")) (list "a" "b"))
(check-expect (create-set (list "a" "b" "a")) (list "b" "a"))

#;
(define (create-set los) empty)

(define (create-set los)
  (cond [(empty? los) empty]
        [else
         (if (member? (first los) (rest los))
             (create-set (rest los))
             (cons (first los) (create-set (rest los))))]))