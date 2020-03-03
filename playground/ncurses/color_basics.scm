#!/usr/bin/env guile
!#

(use-modules (ncurses curses))

(define stdscr (initscr))
(if (has-colors?)
    (begin
      (start-color!)
      (init-pair! 1 COLOR_GREEN COLOR_YELLOW)
      (attr-on! stdscr (logior A_BOLD (color-pair 1)))
      (addstr stdscr "Voila!! In color...")
      (refresh stdscr)
      (sleep 3)
      (endwin)
      0)
    (begin
      (endwin)
      (display "Your terminal does not support color")
      1))
