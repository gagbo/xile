#!/usr/bin/env guile
!#

(use-modules (ncurses curses)
             (ncurses panel))

(define stdscr (initscr))

(let* ((win1 (newwin 5 10 5 5))
       (win2 (newwin 5 10 7 7)))
  (make-panel! win1)
  (make-panel! win2)
  (box win1 (acs-vline) (acs-hline))
  (addstr win1 "box 1"  #:y 1 #:x 1)
  (box win2 (acs-vline) (acs-hline))
  (addstr win2 "box 2" #:y 1 #:x 1)
  (update-panels)
  (doupdate)
  (sleep 1)

  ;; Move panel 1 to the bottom
  (addstr win1 "bottom" #:y 1 #:x 1)
  (bottom-panel win1)
  (update-panels)
  (doupdate)
  (sleep 1)

  ;; Move panel 1 to the top
  (addstr win1 "top   " #:y 1 #:x 1)
  (top-panel win1)
  (update-panels)
  (doupdate)
  (sleep 1)

  ;; Hide panel 1
  (addstr win1 "hide   " #:y 1 #:x 1)
  (hide-panel win1)
  (update-panels)
  (doupdate)
  (sleep 1)

  ;; Unhide panel 1
  (addstr win1 "show   " #:y 1 #:x 1)
  (show-panel win1)
  (update-panels)
  (doupdate)
  (sleep 1)

  ;; Move panel 1
  (addstr win1 "move   " #:y 1 #:x 1)
  (move-panel win1 2 2)
  (update-panels)
  (doupdate)
  (sleep 1))

(endwin)
