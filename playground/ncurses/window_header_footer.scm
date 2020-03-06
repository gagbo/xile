#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ncurses curses))

;; screen is only used to debug arguments in main window
(define (make-header-win screen)
  (let* ((height 1)
         (width (- (cols) 10))
         (startx 5)
         (starty 0))
    (addstr
     screen
     (format #f "Debug : newwin args for header are ~a ~a ~a ~a" height width starty startx)
     #:y 5 #:x 10)
    (derwin screen height width starty startx)))

;; screen is only used to debug arguments in main window
(define (make-footer-win screen)
  (let* ((height 2)
         (width 0)
         (startx 0)
         (starty (- (lines) height)))
    (addstr
     screen
     (format #f "Debug : newwin args for footer are ~a ~a ~a ~a" height width starty startx)
     #:y 6 #:x 10)
    (derwin screen height width starty startx)))

(define (main args)
  (let* ((stdscr (initscr))
         (header-win (make-header-win stdscr))
         (footer-win (make-footer-win stdscr)))

    ;; Window init code
    (raw!)
    (keypad! stdscr #t)
    (noecho!)

    ;; HACK Header and Footer hacks
    (addstr header-win "Arbitrary header text" #:y 0 #:x 5)
    (refresh header-win)

    (addstr footer-win "Arbitrary footer text" #:y 0 #:x 10)
    (refresh footer-win)

    ;; HACK Basic stdscr code
    (addstr stdscr "Type any character to see it in bold\n" #:y 10 #:x 30)
    (let ((ch (getch stdscr)))
      (addstr stdscr "The pressed key is " #:y 11 #:x 30)
      (if (char? ch)
          (addch stdscr (bold ch))
          (addchstr stdscr (bold (keyname ch)))))

    (getch stdscr)

    (endwin)))
