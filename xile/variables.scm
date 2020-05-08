;; coding: utf-8
;; Global variables for Xile.
;; Buffer local variables should be defined in the big buffer record.

(define-module (xile variables)
  #:export (first-file
            languages-available
            themes-available))

;; The first file to open with Xile
(define first-file "not README.org")

;; The list of available languages
(define languages-available #())

;; The list of available themes
(define themes-available #())
