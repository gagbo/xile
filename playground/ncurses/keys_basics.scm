#!/usr/bin/env guile
!#

(use-modules (ncurses curses))

;; Draw the menu on to the window WIN, using the list of CHOICES,
;; highlighting the currently selected entry.
(define (print-menu win highlight choices)
  (let ((x 2)
        (y 2)
        (n-choices (length choices)))

    ;; The menu border
    (box win (acs-vline) (acs-hline))

    ;; The menu entries from CHOICES, with the current entry
    ;; highlighted
    (let loop ((i 0)
               (y y)
               (n n-choices))

      (cond
       ((eqv? highlight i)
        (attr-on! win A_REVERSE)
        (move win y x)
        (addstr win (list-ref choices i))
        (attr-off! win A_REVERSE))
       (else
        (move win y x)
        (addstr win (list-ref choices i))))

      (if (< i (- n-choices 1))
          (loop (+ 1 i)
                (+ 1 y)
                n-choices)
          (refresh win)))))


;; This loop lets the user select a menu. It returns the number of the
;; selected item.
(define (get-selection menu-win highlight choices)
  (let ((n-choices (length choices)))
    (let loop ((highlight highlight)
               (ch (getch menu-win)))

      (cond

       ((eqv? ch KEY_UP)
        ((lambda (h)
           (print-menu menu-win h choices)
           (loop h (getch menu-win)))
         (if (eqv? highlight 0)
             (- n-choices 1)
             (- highlight 1))))

       ((eqv? ch KEY_DOWN)
        ((lambda (h)
           (print-menu menu-win h choices)
           (loop h (getch menu-win)))
         (if (eqv? highlight (- n-choices 1))
             0
             (+ highlight 1))))

       ;; If enter or return is pressed, return the current selected
       ;; menu item
       ((or (eqv? ch #\nl) (eqv? ch KEY_ENTER))
        highlight)

       (else
        (loop highlight (getch menu-win)))))))


(define (main)
  (let ((stdscr(initscr)))
    (cbreak!)
    (noecho!)

    (let* ((menu-width 30)
           (menu-height 10)
           (startx (round (/ (- (cols) menu-width) 2)))
           (starty (round (/ (- (lines) menu-height) 2)))
           (menu-win (newwin menu-height menu-width starty startx))
           (choices '("Choice 1"
                      "Choice 2"
                      "Choice 3"
                      "Choice 4"
                      "Exit"))
           (highlight 0))
      (begin
        (keypad! menu-win #t)
        (print-menu menu-win 0 choices)
        (let ((choice (get-selection menu-win highlight choices)))
          (move stdscr 23 0)
          (addstr stdscr (format #f "You chose ~s ~%"
                        (list-ref choices choice)))
          (clrtoeol stdscr)
          (refresh stdscr)
          (sleep 2)
          (endwin)
          0)))))

(main)
