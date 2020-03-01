#!/usr/bin/env -S guile -s
!#

(use-modules (ncurses curses))

(define stdscr (initscr))
(addstr stdscr "Hello World!!!")
(refresh stdscr)
(getch stdscr)
(endwin)
