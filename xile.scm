#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (ncurses curses)
             (ice-9 threads)            ;; For the join-thread in main
             (xile message)
             (xile json-rpc))

;; Main
(define (main args)
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (port-to-xi (cddr xi-setup))
         (listener (car xi-setup))
         (stdscr (initscr)))

    ;; Window init code
    (raw!)
    (keypad! stdscr #t)
    (noecho!)

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

    ;; (addstr stdscr "Type any character to see it in bold\n")
    ;; (let ((ch (getch stdscr)))
    ;;   (addstr stdscr "The pressed key is ")
    ;;   (if (char? ch)
    ;;       (addch stdscr (bold ch))
    ;;       (addchstr stdscr (bold (keyname ch))))

      (refresh stdscr)
      (getch stdscr)

    ;; TODO : event loop thread instead of joining
    (join-thread listener (current-time))
    (close-port port-to-xi)
    (close-port port-from-xi)
    (endwin)))
