;; coding: utf-8

(define-module (xile application sdl)
  #:use-module (ice-9 threads)
  #:use-module (sdl2)
  #:use-module (sdl2 events)
  #:use-module (sdl2 render)
  #:use-module (sdl2 surface)
  #:use-module (sdl2 ttf)
  #:use-module (sdl2 video)
  #:use-module (xile std process)
  #:use-module (xile std buffer)
  #:use-module (xile sdl window)
  #:use-module (xile sdl input)
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
    ;; There is probably a race condition that needs help debugging
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
               ;; Ignore the modifiers being pushed down
               ((member (keyboard-event-key event)
                        '(left-shift right-shift left-control right-control left-alt right-alt left-gui right-gui))
                (loop (poll-event) key-sequence))
               (else
                (let* ((current-key (keyboard-event->string-sequence event))
                       (current-key-sequence (key-sequence-add key-sequence current-key))
                       (editor-state (eval current-state (resolve-module '(xile editor-states))))
                       (binding (find-binding (assoc-ref editor-state 'keymap) current-key-sequence)))
                  (if binding
                      (begin
                        (when debug-key-presses
                          (format #t "Handled key press (~a state) : received ~a -> ~a~%" (assoc-ref editor-state 'name) current-key-sequence (procedure-name binding)))
                        (binding)
                        (if (eq? (procedure-name binding) 'kill-xile)
                            #f
                            (loop (poll-event) "")))
                      (begin
                       (format #t "Received ~a : key sequence is ~a~%" current-key current-key-sequence)
                       ;; TODO : the code that resets the key-sequence if too long is hidden here. Needs to be
                       ;; in a "brighter" location
                       (loop (poll-event) (if (> 3 (key-sequence-length current-key-sequence))
                                              ""
                                              current-key-sequence))))))))
             (else
              (when event (format #t "Unhandled event : received ~a~%" event))
              (loop (poll-event) key-sequence)))))))

    (ttf-quit)
    (sdl-quit)))
