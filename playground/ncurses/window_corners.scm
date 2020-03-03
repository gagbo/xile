#!/usr/bin/env guile
!#

(use-modules (ncurses curses))

(define stdscr (initscr))

;; Draw a box the hard way
(define (box2 win y x height width)
  ;; top
  (move win y x)
  (addch win (acs-ulcorner))
  (move win y (1+ x))
  (hline win (acs-hline) (- width 2))
  (move win y (+ x width -1))
  (addch win (acs-urcorner))

  ;; left side
  (move win (+ y 1) x)
  (vline win (acs-vline) (- height 2))

  ;; right side
  (move win (+ y 1) (+ x width -1))
  (vline win (acs-vline) (- height 2))

  ;; bottom
  (move win (+ y height -1) x)
  (addch win (acs-llcorner))
  (move win (+ y height -1) (1+ x))
  (hline win (acs-hline) (- width 2))
  (move win (+ y height -1) (+ x width -1))
  (addch win (acs-lrcorner)))

(let* ((stdscr (initscr))
       (height 3)
       (width 10)
       (y (round (/ (- (lines) height) 2)))
       (x (round (/ (- (cols) width) 2))))
  (box2 stdscr y x height width)
  (refresh stdscr)
  (sleep 3)
  (endwin))

