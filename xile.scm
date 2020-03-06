#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (ncurses curses)
             (ice-9 threads)            ;; For the join-thread in main
             (xile message)
             (xile json-rpc))

;; This procedure makes a new window and draws a box
;; around it
;; (define (create-newwin height width starty startx)
;;   ((lambda (win)                        ; Make a lambda proc that
;;      (box win (acs-vline) (acs-hline))  ; Makes a box,
;;      (refresh win)                      ; Draws the window
;;      win)                               ; Returns the window to the caller

;;    (newwin height width starty startx))) ; Create a window and apply it

(define (make-xile-header screen)
  (let* ((height 1)
         (width (- (cols) 10))
         (startx 5)
         (starty 0))
    (derwin screen height width starty startx)))

(define (make-xile-footer screen)
  ;; TODO : 2 rows on the bottom
  ;; top row reverse
  ;; bottom row for the commands later
  ;; return the footer window
  (let* ((height 2)
         (width 0)
         (startx 0)
         (starty (- (lines) height)))
    (derwin screen height width starty startx)))

;; Main
(define (main args)
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (port-to-xi (cddr xi-setup))
         (listener (car xi-setup))
         (stdscr (initscr))
         (xile-header (make-xile-header stdscr))
         (xile-footer (make-xile-footer stdscr))
         )

    ;; Window init code
    (raw!)
    (keypad! stdscr #t)
    (noecho!)

    ;; HACK Header and Footer hacks
    (attr-set! xile-header A_REVERSE)
    (bkgdset! xile-header (inverse #\sp))
    (addstr xile-header "Xile alpha -- header" #:y 0 #:x 5)
    (refresh xile-header)

    (attr-set! xile-footer A_REVERSE)
    (bkgdset! xile-footer (inverse #\sp))
    (addstr xile-footer "Xile alpha -- footer" #:y 0 #:x 10)
    (refresh xile-footer)

    ;; Logging init code
    (set-current-error-port (open "logs/xile-err.log" (logior O_APPEND O_CREAT O_WRONLY)))
    (set-current-output-port (open "logs/xile-out.log" (logior O_APPEND O_CREAT O_WRONLY)))

    ;; Xi init code
    (xile-rpc-send port-to-xi (xile-msg-init '()))
    (let ((msg (xile-msg-new_view '((file_path . "README.org")))))
      (xile-register-callback
       (car msg)
       (lambda (result)
         (format #t "Got ~a for the new_view message ~
                     -> Now I'd like to put the value back into ~
                        a parent environment binding~%" result)))
      (xile-rpc-send port-to-xi msg))

    ;; HACK Basic stdscr code
    (addstr stdscr "Type any character to see it in bold\n" #:y 10 #:x 30)
    (let ((ch (getch stdscr)))
      (addstr stdscr "The pressed key is " #:y 11 #:x 30)
      (if (char? ch)
          (addch stdscr (bold ch))
          (addchstr stdscr (bold (keyname ch)))))

    (getch stdscr)

    ;; TODO : event loop thread instead of joining
    (join-thread listener (current-time))
    (close-port port-to-xi)
    (close-port port-from-xi)
    (endwin)))
