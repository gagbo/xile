;; coding: utf-8
;; Standard library of buffer functions

(define-module (xile std buffer)
  #:use-module (ice-9 optargs)
  #:use-module (xile variables)
  #:export (move_down
            move_up
            scroll-view-down
            scroll-view-up))

(define* (move_down #:key (buffer current-buffer))
  "Move the cursor in BUFFER down one line."
  ((buffer 'move_down)))

(define* (move_up #:key (buffer current-buffer))
  "Move the cursor in BUFFER up one line."
  ((buffer 'move_up)))

(define* (scroll-view-down #:key (buffer current-buffer))
  "Scroll the view BUFFER down one line."
  ((buffer 'scroll-view-down)))

(define* (scroll-view-up #:key (buffer current-buffer))
  "Scroll the view BUFFER up one line."
  ((buffer 'scroll-view-up)))
