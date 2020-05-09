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
            current-theme
            id-to-buffer
            id-to-buffer-guard
            debug-line-cache
            debug-incoming-messages
            listener-stdout
            listener-stderr))

;; The first file to open with Xile
(define-once first-file "not README.org")

;; The list of available languages
(define-once languages-available #())

;; The list of available themes
(define-once themes-available #())

;; Height of the header
(define-once header-height 1)

;; Height of the footer
(define-once footer-height 2)

;; Id to Buffer hashtable
(define-once id-to-buffer (make-hash-table 31))

;; Id to buffer mutex
(define-once id-to-buffer-guard (make-mutex))

;; When #t, display line-cache debugging information
(define-once debug-line-cache #f)

;; When #t, display incoming messages debugging information
(define-once debug-incoming-messages #f)

;; Path to the file containing the stdout of listener process
(define-once listener-stdout "logs/xile-listen-out.log")

;; Path to the file containing the stderr of listener process
(define-once listener-stderr "logs/xile-listen-err.log")

;; The current theme (see xile-theme record)
(define-once current-theme #f)

;; The current status
(define-once status-bar '())
