;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-241) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =========
;; Constants

(define ABSOLUTE0 -272)

;; ===============
;; Data definition

;; CTemperature is Number greate than ABSOLUTE0
;; represents a temperature in Celsius.

;; NEListOfTemperature is one of:
;; - (cons CTemperature empty)
;; - (cons CTemperature NEListOfTemperature)
;; represents a non-empty list of Celisius temperature.

#;
(define (fn-for-neloct neloct)
  (cond [(empty? (rest neloct)) (...)]
        [else
         (... (first neloct)
              (fn-for-neloct (rest neloct)))]))

;; NEListOfBoolean is one of:
;; - (cons Boolean empty)
;; - (cons Boolean NEListOfBoolean)
;; represents a non-empty list of Boolean.

#;
(define (fn-for-nelob nelob)
  (cond [(empty? (rest nelob)) (...)]
        [else
         (... (first nelob)
              (fn-for-nelob (rest nelob)))]))

;; [NEListOfX] is one of:
;; - (cons X empty)
;; - (cons X [NEListOfX])
;; Represent a non-empty list of X.

#;
(define (fn-for-nelox nelox)
  (cond [(empty? (rest nelox)) (...)]
        [else
         (... (first nelox)
              (fn-for-nelox (rest nelox)))]))

;; NEListOfCTemperature is [NEListOf CTemperature]
;; NEListOfBoolean is [NEListOf Boolean]