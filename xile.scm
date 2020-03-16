#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (or (current-filename) ".")))
(use-modules (ncurses curses)
             (ice-9 threads) ;; For the join-thread in main
             (xile message)
             (xile json-rpc)
             (xile curses-window)
             (xile backend-notifications))

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

      ;; Xi init code
      (xile-rpc-send port-to-xi send-mutex (xile-msg-init))

      (begin
        ;; HACK register notification callback
        (xile-register-callback
         'update
         ;; The "update" callback here is just badly extracting the text
         ;; from the first line of updates.
         (lambda (result)
           (let* ((view_id (assoc-ref result "view_id"))
                  (xile-buffer (find-xile-buffer (string->symbol view_id))))
             (format #t "Update : xile-buffer for ~a is ~a~%" view_id xile-buffer)
             (when xile-buffer
               ((xile-buffer 'cb-update) (parse-xi-update result))))))

        (xile-register-callback
         'scroll_to
         (lambda (result)
           (let* ((y (assoc-ref result "line"))
                 (x (assoc-ref result "col"))
                 (view_id (assoc-ref result "view_id"))
                 (xile-buffer (find-xile-buffer (string->symbol view_id))))
             (format #t "Scroll-to : xile-buffer for ~a is ~a~%" view_id xile-buffer)
             (when xile-buffer
               ((xile-buffer 'cb-scroll-to) y x)))
           ))

        (xile-register-callback
         'language_changed
         (lambda (result)
           (format #t "language_changed unimplemented !~%")))

        (xile-register-callback
         'config_changed
         (lambda (result)
           (format #t "config_changed unimplemented !~%")))

        (xile-register-callback
         'available_themes
         (lambda (result)
           (format #t "available_themes unimplemented !~%")))

        (xile-register-callback
         'available_plugins
         (lambda (result)
           (format #t "available_plugins unimplemented !~%")))

        (xile-register-callback
         'available_languages
         (lambda (result)
           (format #t "available_languages unimplemented !~%")))

        ;; HACK open / manipulate file
        (define first-buffer (make-xile-buffer port-to-xi send-mutex "README.org"))
        ((first-buffer 'create-view))
        ((first-buffer 'scroll) 0 (getmaxy (first-buffer 'get-win))))



      ;; Main event loop
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
          ((first-buffer 'scroll) 30 (+ 30 (getmaxy (first-buffer 'get-win))))
          (loop (getch (first-buffer 'get-win))))


         (else
          (clear-footer-text xile-footer)
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
