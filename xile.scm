#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (ncurses curses)
             (ice-9 threads)            ;; For the join-thread in main
             (xile message)
             (xile json-rpc))

(define (make-xile-header)
  (let* ((height 1)
         (width (- (cols) 10))
         (startx 5)
         (starty 0))
    (newwin height width starty startx)))

(define (make-xile-footer)
  ;; TODO : 2 rows on the bottom
  ;; top row reverse
  ;; bottom row for the commands later
  ;; return the footer window
  (let* ((height 2)
         (width 0)
         (startx 0)
         (starty (- (lines) height)))
    (newwin height width starty startx)))

(define (make-xile-main)
  ;; TODO : Get proper height/width params
  (let* ((height (- (lines) 2 1))
         (width 0)
         (startx 0)
         (starty (- (lines) 1)))
    (newwin height width starty startx)))

;; Main
(define (main args)
  ;; This let initializes resources
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (port-to-xi (cddr xi-setup))
         (listener (car xi-setup))
         (stdscr (initscr)))

    ;; Window init code
    (raw!)
    (keypad! stdscr #t)
    (noecho!)

    ;; This let provides an environment with the window handles
    (let ((xile-header (make-xile-header))
          (xile-main (make-xile-main))
          (xile-footer (make-xile-footer)))


      ;; HACK Header and Footer hacks
      (attr-set! xile-header A_REVERSE)
      (bkgdset! xile-header (inverse #\sp))
      (addstr xile-header "Xile alpha -- header" #:y 0 #:x 5)
      (refresh xile-header)

      (attr-set! xile-footer A_REVERSE)
      (bkgdset! xile-footer (inverse #\sp))
      (addstr xile-footer "Xile alpha -- footer" #:y 0 #:x (round (/ (- (cols) 20) 2)))
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

      ;; HACK Basic xile-main code
      (addstr xile-main "Type any character to see it in bold\n" #:y 10 #:x 30)
      (refresh xile-main)
      (let ((ch (getch xile-main)))
        (addstr xile-main "The pressed key is " #:y 11 #:x 30)
        (if (char? ch)
            (addch xile-main (bold ch))
            (addchstr xile-main (bold (keyname ch))))
        (refresh xile-main))

      (move xile-main 0 10)
      (getch xile-main))

    ;; Closing code (give back resources)
    ;; TODO : event loop thread instead of joining
    (join-thread listener (current-time))
    (close-port port-to-xi)
    (close-port port-from-xi)
    (endwin)))
