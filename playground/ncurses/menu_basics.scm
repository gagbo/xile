#!/usr/bin/env guile
-s
!#

(use-modules (srfi srfi-1)
             (ncurses curses)
             (ncurses menu))

(define stdscr (initscr))
(cbreak!)
(noecho!)
(keypad! stdscr #t)

(let* (;; Labels for the menu items
       (names '("Choice 1" "Choice 2" "Choice 3" "Choice 4" "Exit"))
       (descriptions '("Description 1" "Description 2" "Description 3"
                       "Description 4" ""))

       ;; Create menu items for each label
       (my-items (map (lambda (name desc) (new-item name desc))
                      names
                      descriptions))
       ;; Create the menu
       (my-menu (new-menu my-items)))

  ;; Draw the menu
  (move stdscr (- (lines) 2) 0)
  (addstr stdscr "Press 'q' to Quit")
  (post-menu my-menu)
  (refresh stdscr)

  ;; Process the up and down arrow keys.  Break the loop if F1 is
  ;; pressed.  Ignore other keys.
  (let loop ((c (getch stdscr)))
    (cond

     ;; Move down the menu when down arrow is pressed and then loop.
     ((eqv? c KEY_DOWN)
      (begin
        (menu-driver my-menu REQ_DOWN_ITEM)
        (loop (getch stdscr))))

     ;; Move up the menu when the up arrow is pressed and then loop.
     ((eqv? c KEY_UP)
      (begin
        (menu-driver my-menu REQ_UP_ITEM)
        (loop (getch stdscr))))

     ;; When enter is pressed, return the selection and quit.
     ((or (eqv? c KEY_ENTER)
          (eqv? c #\cr)
          (eqv? c #\nl))
      (begin
        (unpost-menu my-menu)
        (move stdscr (- (lines) 4) 0)
        (addstr stdscr
                (format #f "You selected item #~a: ~a"
                        (item-index (current-item my-menu))
                        (item-name (current-item my-menu))))
        (refresh stdscr)
        (sleep 2)))

     ;; If 'Q' or 'q'  is pressed, quit.  Otherwise, loop.
     ((not (or (eqv? c #\Q) (eqv? c #\q)))
      (loop (getch stdscr)))))

  (endwin))
