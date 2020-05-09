#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (ncurses curses)
             (ice-9 threads) ;; For the join-thread in main
             (xile message)
             (xile json-rpc)
             (xile curses-window)
             (xile backend-notifications)
             (xile line-cache)
             (xile callbacks)
             (xile variables))

;; Main
(define (main args)
  ;; This let initializes resources
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (recv-mutex (caddr xi-setup))
         (port-to-xi (cadddr xi-setup))
         (send-mutex (cddddr xi-setup))
         (listener (car xi-setup))
         (stdscr (initscr)))

    ;; Read user configuration
    (load "config/xile.scm")

    ;; Window init code
    (raw!)
    (start-color!)
    (keypad! stdscr #t)
    (noecho!)
    (nonl!)
    (intrflush! #f)

    ;; This let provides an environment with the window handles
    (let ((xile-header (make-xile-header))
          (xile-footer (make-xile-footer)))

      (keypad! xile-header #t)
      (keypad! xile-footer #t)

      ;; Header and Footer init
      (update-header xile-header "Xile alpha -- header")
      (update-footer xile-footer "Xile alpha -- footer")

      ;; HACK Logging init code
      (set-current-error-port (open "logs/xile-err.log" (logior O_APPEND O_CREAT O_WRONLY)))
      (set-current-output-port (open "logs/xile-out.log" (logior O_APPEND O_CREAT O_WRONLY)))

      ;; Register the default callbacks before sending the init message
      ;; (otherwise we might receive notifications before callbacks are
      ;; registered, like available_languages notification)
      (register-default-callbacks)

      ;; Xi init code
      (xile-rpc-send port-to-xi send-mutex (xile-msg-init))

      (begin
        ;; HACK open / manipulate file
        (define first-buffer (make-xile-buffer port-to-xi send-mutex first-file))
        ((first-buffer 'create-view))
        ((first-buffer 'scroll) 0 (getmaxy (first-buffer 'get-win))))

      ;; Main event loop
      (update-footer xile-footer (format #f  "Xile alpha -- ~a" (first-buffer 'get-name)))
      (let loop ((ch (getch (first-buffer 'get-win))))
        (cond
         ((eqv? ch #\q)
          #f)
         ((eqv? ch KEY_DOWN)
          (first-buffer 'move_down)
          (loop (getch (first-buffer 'get-win))))
         ((eqv? ch KEY_UP)
          (first-buffer 'move_up)
          (loop (getch (first-buffer 'get-win))))
         ((eqv? ch KEY_NPAGE)
          ((first-buffer 'scroll-view-down) 1)
          (loop (getch (first-buffer 'get-win))))
         ((eqv? ch KEY_PPAGE)
          ((first-buffer 'scroll-view-up) 1)
          (loop (getch (first-buffer 'get-win))))

         (else
          (clear-footer-text xile-footer)
          (when debug-key-presses
            (format #t "Key press : received ~s~%" ch))
          (addstr xile-footer (format #f "Press q to quit (you pressed ~a)" ch) #:y 1 #:x 0)
          (refresh xile-footer)
          (loop (getch (first-buffer 'get-win)))))))

    ;; Closing code (give back resources)
    (join-thread listener (current-time))
    (with-mutex send-mutex
      (close-port port-to-xi))
    (with-mutex recv-mutex
      (close-port port-from-xi))
    (endwin)))
