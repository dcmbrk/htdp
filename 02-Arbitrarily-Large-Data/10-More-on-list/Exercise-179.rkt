;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-179) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions
;; ================

(define-struct editor [pre post])
;; Editor is (make-editor LO1S LO1S)

#;
(define (fn-for-editor ed)
  (... (fn-for-lo1s (editor-pre ed))
       (fn-for-lo1s (editor-post ed))))

;; LO1S is one of:
;; - empty
;; - (cons 1String LO1S)

#;
(define (fn-for-lo1s lo1s)
  (cond [(empty? lo1s) (...)]
        [else
         (... (first lo1s)
              (fn-for-lo1s (rest lo1s)))]))

;; Functions
;; =========

;; Editor -> Editor
;; creates instance of editor.
(check-expect (create-editor empty empty) (make-editor empty empty))
(check-expect (create-editor (list "a" "b")
                             empty)
              (make-editor (list "a" "b")
                           empty))
(check-expect (create-editor empty
                             (list "c" "d"))
              (make-editor empty
                           (list "c" "d")))
(check-expect (create-editor (list "a" "b")
                             (list "c" "d"))
              (make-editor (list "a" "b")
                           (list "c" "d")))

(define (create-editor pre post)
  (make-editor pre post))

;; Editor -> Editor
;; moves the cursor position one 1String left if can
(check-expect (local [(define ed (create-editor empty
                                                empty))]
                (editor-left ed))
              (create-editor empty
                             empty))
(check-expect (local [(define ed (create-editor (list "b" "a")
                                                empty))]
                (editor-left ed))
              (create-editor (list "a")
                             (list "b")))
(check-expect (local [(define ed (create-editor empty
                                                (list "c" "d")))]
                (editor-left ed))
              (create-editor empty
                             (list "c" "d")))
(check-expect (local [(define ed (create-editor (list "b" "a")
                                                (list "c" "d")))]
                (editor-left ed))
              (create-editor (list "a")
                             (list "b" "c" "d")))

#;
(define (editor-left ed) ed)

(define (editor-left ed)
  (if (empty? (editor-pre ed))
      ed
      (create-editor (rest (editor-pre ed))
                     (cons (first (editor-pre ed))
                           (editor-post ed)))))


;; Editor -> Editor
;; moves the cursor position one 1String right if can
(check-expect (local [(define ed (create-editor empty
                                                empty))]
                (editor-right ed))
              (create-editor empty
                             empty))
(check-expect (local [(define ed (create-editor (list "b" "a") empty))]
                (editor-right ed))
              (create-editor (list "b" "a")
                             empty))
(check-expect (local [(define ed (create-editor empty (list "c" "d")))]
                (editor-right ed))
              (create-editor (list "c")
                             (list "d")))
(check-expect (local [(define ed (create-editor (list "b" "a")
                                                (list "c" "d")))]
                (editor-right ed))
              (create-editor (list "c" "b" "a")
                             (list "d")))

(define (editor-right ed)
  (if (empty? (editor-post ed))
      ed
      (create-editor (cons (first (editor-post ed))
                           (editor-pre ed))
                     (rest (editor-post ed)))))


;; Editor -> Editor
;; deletes a 1String to the left of the cursor if can
(check-expect (local [(define ed (create-editor empty empty))]
                (editor-delete ed))
              (create-editor empty empty))
(check-expect (local [(define ed (create-editor (list "b" "a")
                                                empty))]
                (editor-delete ed))
              (create-editor (list "a")
                             empty))
(check-expect (local [(define ed (create-editor empty
                                                (list "c" "d")))]
                (editor-delete ed))
              (create-editor empty
                             (list "c" "d")))
(check-expect (local [(define ed (create-editor (list "b" "a")
                                                (list "c" "d")))]
                (editor-delete ed))
              (create-editor (list "a")
                             (list "c" "d")))

;(define (editor-delete ed) ed)

(define (editor-delete ed)
  (if (empty? (editor-pre ed))
      ed
      (create-editor (rest (editor-pre ed))
                     (editor-post ed))))
