;; coding: utf-8

(define-module (xile application sdl)
  #:use-module (ice-9 threads)
  #:use-module (sdl2)
  #:use-module (sdl2 events)
  #:use-module (sdl2 render)
  #:use-module (sdl2 surface)
  #:use-module (sdl2 ttf)
  #:use-module (sdl2 video)
  #:use-module (xile sdl window)
  #:use-module (xile sdl callbacks)
  #:use-module (xile buffer)
  #:use-module (xile editor-states)
  #:use-module (xile xi-protocol json-rpc) ; for xile-rpc-send
  #:use-module (xile xi-protocol message)  ; for xile-msg-init
  #:use-module (xile variables)            ; for first-file
  #:export (start-xile-in-sdl))

(define (start-xile-in-sdl xi-setup)
  "STUB - Start and handle Xile in a curses window using XI-SETUP."
  (let ((port-from-xi (cadr xi-setup))
        (recv-mutex (caddr xi-setup))
        (port-to-xi (cadddr xi-setup))
        (send-mutex (cddddr xi-setup))
        (listener (car xi-setup)))

    (sdl-init '(video events))
    (ttf-init)

    ;; FIXME : Sometimes this bugs out, as in :
    ;; - the window only flashes quickly
    ;; - After the terminal doesn't give back control and doesn't pipe anything to xile process.
    (call-with-window
        (make-xile-window)
      (lambda (window)
        (let* ((win-grid-size (grid-size window (xile-sdl-font)))
               (grid-width (car win-grid-size))
               (grid-height (cdr win-grid-size)))

          ;; Debug statement
          (format #t "Window size is ~a Grid unit is ~a -> ~a ~%" (window-size window) (grid-unit-size (xile-sdl-font)) (grid-size window (xile-sdl-font)))
        
          ;; Register the default callbacks before sending the init message
          ;; (otherwise we might receive notifications before callbacks are
          ;; registered, like available_languages notification)
          (register-default-sdl-callbacks window)

          ;; Xi init code
          (xile-rpc-send port-to-xi send-mutex (xile-msg-init))

          ;; open / manipulate file
          (set! current-buffer (make-xile-buffer port-to-xi send-mutex first-file))
          ((current-buffer 'create-view))
          ((current-buffer 'scroll) 0 grid-height)
          (let loop ((event (poll-event))
                     (key-sequence ""))
            (cond
             ((quit-event? event)
              #f)
             ((keyboard-down-event? event)
              (cond
               ((eq? (keyboard-event-key event) 'j)
                ((current-buffer 'scroll-view-down))
                (loop (poll-event) ""))
               ((eq? (keyboard-event-key event) 'q)
                #f)
               ((eq? (keyboard-event-key event) 'k)
                ((current-buffer 'scroll-view-up))
                (loop (poll-event) ""))
               (else
                (format #t "Unhandled event : received ~a~%" event)
                (loop (poll-event) key-sequence))))
             (else
              (when event (format #t "Unhandled event : received ~a~%" event))
              (loop (poll-event) key-sequence)))))))

    (ttf-quit)
    (sdl-quit)))
