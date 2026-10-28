;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-131) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ListOfBoolean is one of:
;; - '()
;; - (cons Boolean ListOfBoolean)
;; Represents list of Boolean values

(cons #false
      (cons #true
            (cons #true '())))