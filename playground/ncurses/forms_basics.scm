#!/usr/bin/env guile
!#

(use-modules (srfi srfi-1)
             (ncurses curses)
             (ncurses form))

;; Initialize curses
(define stdscr (initscr))
(cbreak!)
(noecho!)
(keypad! stdscr #t)

;; Initialize the fields
(define field (list
               (new-field 1 10 4 18 0 0)
               (new-field 1 10 6 18 0 0)))

;; Set field options
;; Print a line for the options
(set-field-back! (first field) A_UNDERLINE)
;; Don't go to the next field when this field is filled up
(field-opts-off! (first field) O_AUTOSKIP)
(set-field-back! (second field) A_UNDERLINE)
(field-opts-off! (second field) O_AUTOSKIP)

;; Create the new form and post it
(define my-form (new-form field))
(post-form my-form)
(refresh stdscr)

(addstr stdscr "Value 1:" #:y 4 #:x 10)
(addstr stdscr "Value 2:" #:y 6 #:x 10)
(refresh stdscr)

;; Loop through to get user requests
(let loop ((ch (getch stdscr)))
  (if (not (eqv? ch (key-f 1)))
      (cond
       ((eqv? ch KEY_DOWN)
        (begin
          ;; Go to the end of the next field
          (form-driver my-form REQ_NEXT_FIELD)
          (form-driver my-form REQ_END_LINE)
          (loop (getch stdscr))))
       ((eqv? ch KEY_UP)
        (begin
          ;; Go to the end of the previous field
          (form-driver my-form REQ_PREV_FIELD)
          (form-driver my-form REQ_END_LINE)
          (loop (getch stdscr))))
       (else
        (begin
          ;; Print any normal character
          (form-driver my-form ch)
          (loop (getch stdscr)))))))

;; Unpost the form
(unpost-form my-form)

(endwin)
