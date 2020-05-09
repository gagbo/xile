;; coding: utf-8
;; Global variables for Xile.
;; Buffer local variables should be defined in the big buffer record.

(define-module (xile variables)
  #:use-module (ice-9 threads)
  #:export (first-file
            languages-available
            themes-available
            header-height
            footer-height
            id-to-buffer
            id-to-buffer-guard
            debug-line-cache
            debug-incoming-messages))

;; The first file to open with Xile
(define first-file "not README.org")

;; The list of available languages
(define languages-available #())

;; The list of available themes
(define themes-available #())

;; Height of the header
(define header-height 1)

;; Height of the footer
(define footer-height 2)

;; Id to Buffer hashtable
(define id-to-buffer (make-hash-table 31))

;; Id to buffer mutex
(define id-to-buffer-guard (make-mutex))

;; When #t, display line-cache debugging information
(define debug-line-cache #f)

;; When #t, display incoming messages debugging information
(define debug-incoming-messages #f)
