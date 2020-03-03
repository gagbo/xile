#!/usr/bin/env guile
!#

(use-modules (ncurses curses))

;; This procedure makes a new window and draws a box
;; around it
(define (create-newwin height width starty startx)
  ((lambda (win)			; Make a lambda proc that
     (box win (acs-vline) (acs-hline))	; Makes a box,
     (refresh win)			; Draws the window
     win)				; Returns the window to the caller

   (newwin height width starty startx))) ; Create a window and apply it
                                        ; to the lambda function

;; This procedure erases the box around a window and then deletes it
(define (destroy-win win)
  (let ((s (normal #\sp)))
    (border win s s s s s s s s)	; Draw a box of spaces
    (refresh win)
    (delwin win)))

;; This prodecure deletes a window than then draw a new one someplace
;; else
(define (move-win win height width starty startx)
  (destroy-win win)
  (create-newwin height width starty startx))


;; Program Begins
(define stdscr (initscr))		; Start curses
(cbreak!)				; Line buffering disabled
(keypad! stdscr #t)			; Check for function keys


(let* ((height 3)
       (width 10)
       (starty (round (/ (- (lines) height) 2)))
       (startx (round (/ (- (cols) width) 2))))

  (addstr stdscr "Press F1 to exit")
  (refresh stdscr)
  (let loop ((starty starty)
             (startx startx)
             (my-win (create-newwin height width starty startx))
             (ch (getch stdscr)))
    (cond
     ((eqv? ch KEY_LEFT)
      (loop starty
            (- startx 1)
            (move-win my-win height width starty (- startx 1))
            (getch stdscr)))

     ((eqv? ch KEY_RIGHT)
      (loop starty
            (+ startx 1)
            (move-win my-win height width starty (+ startx 1))
            (getch stdscr)))

     ((eqv? ch KEY_UP)
      (loop (- starty 1)
            startx
            (move-win my-win height width (- starty 1) startx)
            (getch stdscr)))

     ((eqv? ch KEY_DOWN)
      (loop (+ starty 1)
            startx
            (move-win my-win height width (+ starty 1) startx)
            (getch stdscr)))

     ((eqv? ch (key-f 1))
      #f)

     (else
      (loop starty startx my-win (getch stdscr)))))

  (endwin))
