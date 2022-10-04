#!/usr/bin/env -S guile -e main -s
!#
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))
(use-modules (xile application))

(define (main args)
  (run-xile-demo-curses))
