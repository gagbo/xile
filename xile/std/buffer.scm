;; coding: utf-8
;; Standard library of buffer functions

(define-module (xile std buffer)
  #:use-module (ice-9 optargs)
  #:use-module (xile variables)
  #:export (move_down
            move_up
            scroll-view-down
            scroll-view-up
            self-insert-factory))

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

(define* (self-insert-factory ch)
  "Return a procedure that inserts CH COUNT times in BUFFER.
For ease of usage in insert-mode keymap."
  ;; TODO : try to use a caching system if keys are hit multiple times
  (let ((ret-proc
         (lambda* (#:key (count 1) (buffer current-buffer))
           (format #t "SELF INSERT : inserting ~a ~a times in ~a~%" ch count buffer)
           ((buffer 'insert) ch))))
    (set-procedure-property! ret-proc 'name (string-concatenate (list "self-insert-" ch)))
    (set-procedure-property! ret-proc 'documentation "Insert COUNT times the typed keys in BUFFER.")
    ret-proc))
