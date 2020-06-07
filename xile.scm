#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (or (current-filename) ".")))
(use-modules (xile application curses)
             (xile application sdl)
             (xile xi-protocol json-rpc) ; xile-setup
             (xile variables))           ; Variables

;; Main
(define (main args)
  ;; Read user configuration
  (load "config/xile.scm")

  ;; HACK Logging init code
  (set-current-error-port (open "logs/xile-err.log" (logior O_APPEND O_CREAT O_WRONLY)))
  (set-current-output-port (open "logs/xile-out.log" (logior O_APPEND O_CREAT O_WRONLY)))

  ;; (start-xile-in-curses (xile-setup))
  (start-xile-in-sdl (xile-setup)))
