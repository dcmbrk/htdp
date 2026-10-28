;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-200) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(require 2htdp/itunes)

;; Data definitions
;; ================

(define-struct date
  [year month day hour minute second])
;; Date is (make-date Number Number Number Number Number)
;; Represent an instance records: year, month, day, hour, minute, second


#;
(define (fn-for-date date)
  (... date-year
       date-month
       date-day
       date-hour
       date-minute
       date-second))

(define-struct track
  [name artist album time# track# added play# played])
;; Track is (make-track String String String Number Number Date Number Date)
;; Represent an instance records: title, artist, album, playing time, position in album, date, replay often, last played.



#;
(define (fn-for-track track)
  (... track-name
       track-artist
       track-album
       track-time#
       track-track#
       track-added
       track-play#
       track-played))

;; Tracks is one of:
;; - empty
;; - (cons Track Tracks)

#;
(define (fn-for-tracks tracks)
  (cond [(empty? tracks) (...)]
        [else
         (... (fn-for-track (first tracks))
              (fn-for-tracks (rest tracks)))]))

;; Functions
;; =========

;; Any Any Any Any Any Any Any Any -> Track or #false
;; creates an instance of Track from valid arguments otherwise produces #false.

(define (create-track name artist album time# track# added play# played)
  (make-track name artist album time# track# added play# played))

;; Any Any Any Any Any Any -> Date or #false
;; creates an instance of Date from valid arguments otherwise produces #false.

(define (create-date year month day hour minute second)
  (make-date year month day hour minute second))

;; String -> Tracks
;; creates a list-of-tracks from text in file name (an XML export from iTunes)

#;
(define (read-itunes-as-tracks file-name)
  ...)
;; !!!

;; Constants
;; =========

(define ITUNES-LOCATION "itunes.xml")
#;
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))
;; !!!


;; DATE
(define DATE-1 (create-date 2020 5 15 14 30 0))
(define DATE-2 (create-date 2023 11 1 9 15 45))

;; TRACK
(define TRACK-1
  (create-track "Wild Child" "Enya" "A Day Without Rain" 227996 2 DATE-1 20 DATE-2))
(define TRACK-2
  (create-track "Only Time" "Enya" "A Day Without Rain" 218000 3 DATE-1 15 DATE-2))

;; TRACKS
(define TRACKS-0 empty)
(define TRACKS-1 (list TRACK-1))
(define TRACKS-2 (list TRACK-1 TRACK-2))

;; Functions
;; =========

;; Tracks -> Number
;; produces total amount of play time.
(check-expect (total-time empty) 0)
(check-expect (total-time TRACKS-1) (track-time# TRACK-1))
(check-expect (total-time TRACKS-2)
              (+ (track-time# TRACK-1)
                 (track-time# TRACK-2)))

;(define (total-time tracks) 0)

(define (total-time tracks)
  (cond [(empty? tracks) 0]
        [else
         (+ (track-time# (first tracks))
            (total-time (rest tracks)))]))