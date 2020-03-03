#!/usr/bin/env guile
-s
!#

(use-modules (srfi srfi-1)
             (ncurses curses)
             (ncurses menu))

(define stdscr (initscr))
(start-color!)
(cbreak!)
(noecho!)
(keypad! stdscr #t)
(init-pair! 1 COLOR_RED COLOR_BLACK)

(let* (;; Labels for the menu items
       (names '("Choice 1" "Choice 2" "Choice 3" "Choice 4" "Exit"))
       (descriptions '("Description 1" "Description 2" "Description 3"
                       "Description 4" ""))

       ;; Create menu items for each label
       (my-items (map (lambda (name desc) (new-item name desc))
                      names
                      descriptions))
       ;; Create the menu
       (my-menu (new-menu my-items))

       ;; Make a windows to hold the menu
       (my-menu-win (newwin 10 40 4 4)))

  (keypad! my-menu-win #t)

  ;; Set the main window and subwindow
  (set-menu-win! my-menu my-menu-win)
  (set-menu-sub! my-menu (derwin my-menu-win 6 38 3 1))

  ;; Set the menu mark string
  (set-menu-mark! my-menu " * ")

  ;; Print a border around the main window, and a title
  (box my-menu-win 0 0)

  (attr-on! my-menu-win (color-pair 1))
  (move my-menu-win 1 16)
  (addstr my-menu-win "My Menu")
  (attr-off! my-menu-win (color-pair 1))

  (move my-menu-win 2 0)
  (addch my-menu-win (acs-ltee))
  (move my-menu-win 2 1)
  (hline my-menu-win (acs-hline) 38)
  (move my-menu-win 2 39)
  (addch my-menu-win (acs-rtee))

  (move stdscr (- (lines) 2) 0)
  (addstr stdscr "F1 to exit")
  (refresh stdscr)

  ;; Post the menu
  (post-menu my-menu)
  (refresh my-menu-win)

  ;; Draw the menu
  (move stdscr (- (lines) 2) 0)
  (addstr stdscr "F1 to Exit")

  ;; Process the up and down arrow keys.  Break the loop if F1 is
  ;; pressed.  Ignore other keys.
  (let loop ((c (getch my-menu-win)))
    (cond

     ;; Move down the menu when down arrow is pressed and then loop.
     ((eqv? c KEY_DOWN)
      (begin
        (menu-driver my-menu REQ_DOWN_ITEM)
        (loop (getch my-menu-win))))

     ;; Move up the menu when the up arrow is pressed and then loop.
     ((eqv? c KEY_UP)
      (begin
        (menu-driver my-menu REQ_UP_ITEM)
        (loop (getch my-menu-win))))

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

     ;; If F1 is pressed, quit.  Otherwise, loop.
     ((not (eqv? c (key-f 1)))
      (loop (getch stdscr)))))

  (endwin))
