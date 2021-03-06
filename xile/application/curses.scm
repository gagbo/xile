;; coding: utf-8
(define-module (xile application curses)
  #:use-module (ice-9 threads)
  #:use-module (ncurses curses)
  #:use-module (xile buffer)
  #:use-module (xile curses window)
  #:use-module (xile curses callbacks)
  #:use-module (xile editor-states)
  #:use-module (xile xi-protocol json-rpc) ; for xile-rpc-send
  #:use-module (xile xi-protocol message)  ; for xile-msg-init
  #:use-module (xile variables)            ; for first-file
  #:export (start-xile-in-curses))

(define (start-xile-in-curses xi-setup)
  "Start and handle Xile in a curses window using XI-SETUP."
  (let ((port-from-xi (cadr xi-setup))
        (recv-mutex (caddr xi-setup))
        (port-to-xi (cadddr xi-setup))
        (send-mutex (cddddr xi-setup))
        (listener (car xi-setup))
        (stdscr (initscr)))

    ;; Window init code
    (raw!)
    (start-color!)
    (keypad! stdscr #t)
    (noecho!)
    (nonl!)
    (intrflush! #f)

    ;; This let provides an environment with the window handles
    (let ((xile-header (make-xile-header))
          (xile-footer (make-xile-footer))
          (xile-main (make-xile-main)))

      (keypad! xile-header #t)
      (keypad! xile-footer #t)
      (keypad! xile-main #t)
      (curs-set 1)

      ;; Header and Footer init
      (update-header xile-header "Xile alpha -- header")
      (update-footer xile-footer "Xile alpha -- footer")

      ;; Register the default callbacks before sending the init message
      ;; (otherwise we might receive notifications before callbacks are
      ;; registered, like available_languages notification)
      (register-default-curses-callbacks xile-header xile-main xile-footer)

      ;; Xi init code
      (xile-rpc-send port-to-xi send-mutex (xile-msg-init))

      ;; HACK open / manipulate file
      (set! current-buffer (make-xile-buffer port-to-xi send-mutex first-file))
      (update-footer xile-footer (format #f  "Xile alpha -- ~a" (current-buffer 'get-name)))
      ((current-buffer 'create-view))
      ((current-buffer 'scroll) 0 (getmaxy xile-main))

      ;; Main event loop
      ;; TODO(curses) : This code below does not print the buffer in pristine state when opened....
      ;; Actually, it looks like the xile-main window is always "one key press" behind regarding
      ;; inforation actually displayed (play with [UP] [DOWN] to see the problem
      (let loop ((key-sequence (encode-key-to-string-sequence (getch xile-main))))
        (let ((binding (find-binding (assoc-ref current-state 'keymap) key-sequence)))
          (cond
           ((equal? binding 'quit)
            #f)
           (binding
            (when debug-key-presses
              (format #t "Handled key press : received ~a -> ~a~%" key-sequence binding))
            ((current-buffer binding))
            (loop (encode-key-to-string-sequence (getch xile-main))))
           (else
            (clear-footer-text xile-footer)
            (when debug-key-presses
              (format #t "Unhandled key press : received ~a~%" key-sequence))
            (addstr xile-footer (format #f "Press q to quit (you pressed ~a)" key-sequence) #:y 1 #:x 0)
            (refresh xile-footer)
            ;; Before sending the concat of string sequences, we need to find a way to start the
            ;; sequence from scratch when we are sure that there are no binding with the same prefix

            (loop (encode-key-to-string-sequence (getch xile-main))))))))

    ;; Closing code (give back resources)
    (join-thread listener (current-time))
    (with-mutex send-mutex
      (close-port port-to-xi))
    (with-mutex recv-mutex
      (close-port port-from-xi))
    (endwin)))
