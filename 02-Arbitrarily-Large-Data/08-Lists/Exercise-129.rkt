;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Exercise-129) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A list of planets in Solar System 
(cons "Neptune"
      (cons "Uranus"
            (cons "Saturn"
                  (cons "Jupiter"
                        (cons "Mars"
                              (cons "Earth"
                                    (cons "Venus"
                                          (cons "Mercury"
                                                '()))))))))
;; A list of meals
(cons "Steak"
      (cons "French fries"
            (cons "Beans"
                  (cons "Bread"
                        (cons "Water"
                              (cons "Brie cheese"
                                    (cons "Ice cream"
                                          '())))))))
;; A list of colors
(cons "Red"
      (cons "Green"
            (cons "Blue"
                  '())))