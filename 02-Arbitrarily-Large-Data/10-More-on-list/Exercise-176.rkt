;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-176) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; constant
;; ========

(define matrix (list (list 11 12)    ; | 11 12 |
                     (list 21 22)))  ; | 21 22 |

;; Data definition
;; ===============

;; Matrix is one of:
;; - (cons Row empty)
;; - (cons Row Matrix)
;; constraint: the number of rows must equal to size of a row.

;; Row is one of:
;; - empty
;; - (cons Number Row)


;; Functions
;; =========

;; Matrix -> Matrix
;; transposes the matrix along the diagnal.
(check-expect (transpose (list (list 11 12)
                               (list 21 22)))
              (list (list 11 21)
                    (list 12 22)))

#;
(define (transpose matrix) matrix)

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

;; Matrix -> Row
;; produces first column of matrix.
(check-expect (first* (list (list 11)))
              (list 11))
(check-expect (first* (list (list 11 12)
                            (list 21 22)))
              (list 11 21))
(check-expect (first* (list (list 11 11 11)
                            (list 22 22 22)
                            (list 33 33 33)))
              (list 11 22 33))

(define (first* matrix)  ;matrix is LOLON;
                         ;OP     is   LON
  (cond [(empty? matrix) empty]
        [else
         (cons (first (first matrix))        ;LON
               (first* (rest matrix)))]))    ;LON

;; Matrix -> Matrix
;; produce matrix without first column.
(check-expect (rest* (list (list 11 12)
                           (list 21 22)))
              (list (list 12)
                    (list 22)))
(check-expect (rest* (list (list 11 11 11)
                           (list 22 22 22)
                           (list 33 33 33)))
              (list (list 11 11)
                    (list 22 22)
                    (list 33 33)))

(define (rest* matrix)
  (cond [(empty? matrix) empty]
        [else
         (cons (rest (first matrix))
               (rest* (rest matrix)))]))

