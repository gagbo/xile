#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ice-9 popen)
             (ice-9 threads)
             ;; (ice-9 getopt-long) ;; for CLI options
             (ice-9 suspendable-ports)
             (ice-9 rdelim)
             (json)
             (ncurses curses))


(install-suspendable-ports!)

;; Debugging / Logging
(define (xile--debug severity message)
  (write-line (string-append severity ": "  message) (current-error-port)))

(define (xile--debug-info message)
  (xile--debug "INFO" message))

;; Xile protocol messages serialization / sending
(define (xile--msg-init param-list)
  (scm->json-string `((method . "client_started") (params . ,param-list))))

(define (xile--msg-send port message)
  (xile--debug-info (string-append "Sending : " message))
  (write-line message port))

;; Xile protocol messages deserialization / receiving
(define (xile--msg-read port)
  (xile--debug-info "waiting for one line")
  (xile--msg-dispatch (json->scm port)))

;; Here the message is parsed JSON. To assert
(define (xile--msg-dispatch message)
  (write-line message (current-error-port)))

(define (xile--msg-handler port)
  (while (not (port-closed? port))
    (xile--msg-read port)))

;; Socket opening function
(define (xile--open path)
  (let* ((xi-pipes (pipe)))
    (setvbuf (car xi-pipes) 'line)
    (setvbuf (cdr xi-pipes) 'line)
    ;; Tried with-output-to-port but it didn't work for some reason
    ;; and sent the output of xi-core process to stdout instead
    (parameterize ((current-output-port (cdr xi-pipes))
                   (current-error-port (open "logs/xi-core.log" (logior O_CREAT O_WRONLY))))
      (let ((from-xi (car xi-pipes))
            (to-xi (open-output-pipe path)))
        (cons from-xi to-xi)))))

(define (xile-setup)
  (let* ((xi-proc (xile--open "xi-editor/rust/target/release/xi-core"))
         (port-from-xi (car xi-proc))
         (port-to-xi (cdr xi-proc))
         (init-client (xile--msg-init '()))
         (listener (make-thread xile--msg-handler port-from-xi)))
    (cons listener (cons port-from-xi port-to-xi))))

;; Main
(define (main args)
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (port-to-xi (cddr xi-setup))
         (listener (car xi-setup)))

    (set-current-error-port (open "logs/xile-err.log" (logior O_CREAT O_WRONLY)))
    (set-current-output-port (open "logs/xile-out.log" (logior O_CREAT O_WRONLY)))

    ;; (define stdscr (initscr))
    ;; (raw!)
    ;; (keypad! stdscr #t)
    ;; (noecho!)

    ;; Xi init code
    (xile--msg-send port-to-xi (xile--msg-init '()))

    ;; (addstr stdscr "Type any character to see it in bold\n")
    ;; (let ((ch (getch stdscr)))
    ;;   (addstr stdscr "The pressed key is ")
    ;;   (if (char? ch)
    ;;       (addch stdscr (bold ch))
    ;;       (addchstr stdscr (bold (keyname ch))))

    ;;   (refresh stdscr)
    ;;   (getch stdscr)
    ;;   (endwin))
    ;; TODO : event loop thread instead of joining
    (join-thread listener (+ 1 (current-time)))
    (close-port port-to-xi)
    (close-port port-from-xi)))
