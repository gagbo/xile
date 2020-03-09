#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (mod))

(define (main args)
  (test-func "the correct version" "so"))
