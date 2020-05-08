;; coding: utf-8

(define-module (xile curses-window)
  #:use-module (xile message)
  #:use-module (xile json-rpc)
  #:use-module (xile backend-notifications)
  #:use-module (xile line-cache)
  #:use-module (xile buffer-state)
  #:use-module (xile variables)
  #:use-module (ncurses curses)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (make-xile-buffer
            find-xile-buffer
            make-xile-footer
            update-footer
            clear-footer-text
            make-xile-header
            update-header
            make-xile-main))

(define (make-xile-header)
  "Return a window suitable for a Xile header."
  (let* ((height header-height)
         (width (- (cols) 10))
         (startx 5)
         (starty 0))
    ((lambda (win)
       (attr-set! win A_REVERSE)
       (bkgdset! win (inverse #\sp))
       win)
     (newwin height width starty startx))))

(define (make-xile-footer)
  "Return a window suitable for a Xile footer."
  (let* ((height footer-height)
         (width 0)
         (startx 0)
         (starty (- (lines) height)))
    ((lambda (win)
       (attr-set! win A_REVERSE)
       win)
     (newwin height width starty startx))))

(define (update-header header-win text)
  "Update the header HEADER-WIN with TEXT."
  ;; TODO Make the refresh optional
  (addstr header-win text #:y 0 #:x 10)
  (refresh header-win))

(define (update-footer footer-win text)
  "Update the footer window FOOTER-WIN with TEXT."
  ;; TODO Make the refresh optional

  ;; HACK : bkdgset! scope assumes the old value is (normal #\sp) and hardcodes it in the
  ;; function.
  ;; It would be better if the value was captured in a let-binding
  (bkgdset! footer-win (inverse #\sp))
  (addstr footer-win text #:y 0 #:x (round (/ (- (cols) (string-length text)) 2)))
  (bkgdset! footer-win (normal #\sp))
  (refresh footer-win))

(define (clear-footer-text footer-win)
  "Clear the text in the footer window FOOTER-WIN."
  ;; TODO Make the refresh optional
  (move footer-win 1 0)
  (clrtobot footer-win)
  (refresh footer-win))

(define (make-xile-main)
  "Return a window suitable to hold a buffer."
  ;; TODO : Get proper height/width params
  ;; 2 is (getmaxy xile-footer-win) and 1 is (getmaxy xile-header-win)
  ;; But HOW can I get these into scope properly for evaluation ?
  (let* ((height (- (lines) 2 1))
         (width 0)
         (startx 0)
         (starty 1))
    (newwin height width starty startx)))

(define (find-xile-buffer view_id)
      "Find a buffer from VIEW_ID. Return #f if VIEW_ID is not registered yet."
      (with-mutex id-to-buffer-guard
        (hashq-ref id-to-buffer view_id)))

(define* (make-xile-buffer port-to-xi send-mutex #:optional file_path)
    "Create a buffer with PORT-TO-XI used to send messages to xi-core process.
SEND-MUTEX protects access to PORT-TO-XI

Optional FILE_PATH can be given to set the view to point to a file.

The dispatching of the returned lambda can be checked in source code.
- cb* messages/procedures regard callbacks to back-end notifications
- Other messages/procedures usually wrap message sending to xi
- A few accessors for the variables in the closures are added

Opening multiple buffers pointing to the same FILE_PATH is undefined behaviour,
as of 2020-03-09, xi doesn't handle multiple views of a single file."
    (let* ((bufstate (make-xile-buffer-state
                      (make-mutex) #f file_path (make-xile-main)
                      (make-mutex) port-to-xi send-mutex #t
                      (make-xi-line-cache #() 0 0) #f '()))
           (current-view (cons 0 (getmaxy (xile-buffer-state-bufwin bufstate))))
           (to-xi (xile-buffer-state-to-xi bufstate))
           (to-xi-guard (xile-buffer-state-to-xi-guard bufstate))
           (bufwin-guard (xile-buffer-state-win-guard bufstate))
           (bufstate-guard (xile-buffer-state-guard bufstate)))

      (define dispatch #f)    ; dispatch acts as "this" in OOP-languages

      ;; TODO : Refactor the notification sending out of buffer.
      ;; This is the most stateless part we can start with.
      (define (rpc-send-notif notif)
        "Send the notification NOTIF to Xi through RPC."
        (xile-rpc-send to-xi to-xi-guard notif))

      (define (scroll min-line max-line)
        "Send Scroll[MIN-LINE MAX-LINE]."
        (rpc-send-notif (xile-msg-edit-scroll (xile-buffer-state-view_id bufstate) min-line max-line)))

      (define (scroll-view-down lines)
        "Scroll the view down LINES lines."
        (let ((current-min-line (car current-view))
              (current-max-line (cdr current-view)))
          (set! current-view (cons (+ current-min-line lines) (+ current-max-line lines)))
          (scroll (car current-view) (cdr current-view))))

      (define (scroll-view-up lines)
        "Scroll the view up LINES lines."
        (let* ((current-min-line (car current-view))
               (current-max-line (cdr current-view))
               (max-scrolled-lines (min lines current-min-line)))
          (set! current-view (cons (- current-min-line max-scrolled-lines) (- current-max-line max-scrolled-lines)))
          (scroll (car current-view) (cdr current-view))))

      (define (move_down)
        "Move the cursor down."
        (rpc-send-notif (xile-msg-edit-move_down (xile-buffer-state-view_id bufstate))))

      (define (move_up)
        "Move the cursor up."
        (rpc-send-notif (xile-msg-edit-move_up (xile-buffer-state-view_id bufstate))))

      (define (create-view)
        " \"\" Constructor \"\" => sends a \"new_view\" message and sets the attribute in the callback. "
        (keypad! (xile-buffer-state-bufwin bufstate) #t)
        (curs-set 1)
        (let ((msg (xile-msg-new_view #:file_path file_path))
              (wait-for-id (make-condition-variable))
              (guard-wait (make-mutex)))
          (xile-register-callback
           (car msg)
           (lambda (result)
             (lock-mutex id-to-buffer-guard)
             (when (and (xile-buffer-state-view_id bufstate) (hashq-get-handle id-to-buffer (xile-buffer-state-view_id bufstate)))
               hashq-remove! id-to-buffer (xile-buffer-state-view_id bufstate))
             (hashq-set! id-to-buffer (string->symbol result) dispatch)
             (unlock-mutex id-to-buffer-guard)
             (set-xile-buffer-state-view_id bufstate result)
             (signal-condition-variable wait-for-id)))
          ;; HACK : using a condvar here means we need to lock/unlock a mutex.
          ;; Seems weird
          (xile-rpc-send to-xi to-xi-guard msg)
          (with-mutex id-to-buffer-guard
            (wait-condition-variable wait-for-id id-to-buffer-guard))))

      (define (draw-buffer)
        "Draw the buffer in its window and refresh the window (redisplay code)."
        (with-mutex bufwin-guard
          (let* ((cache (xile-buffer-state-line_cache bufstate))
                 (inv-before (xi-line-cache-invalid_before cache))
                 (inv-after (xi-line-cache-invalid_after cache))
                 (window-lines (getmaxy (xile-buffer-state-bufwin bufstate))))
            (format (current-output-port) "Drawing between ~a and ~a~%" inv-before inv-after)
            (begin
              (vector-for-each
               (lambda (i line)
                 (when (and (>= i inv-before) (< i inv-after))
                   (if (and (xi-line-valid line) (>= (xi-line-ln line) (1+ (car current-view))))
                       (addstr (xile-buffer-state-bufwin bufstate)
                               (format #f "~a" (xi-line-text line))
                               #:y (- (xi-line-ln line) (1+ (car current-view))) #:x 0))))
               (xi-line-cache-lines (xile-buffer-state-line_cache bufstate)))

              (when (xile-buffer-state-cursor bufstate)
                (move (xile-buffer-state-bufwin bufstate)
                      (car (xile-buffer-state-cursor bufstate))
                      (cdr (xile-buffer-state-cursor bufstate)))))

            (refresh (xile-buffer-state-bufwin bufstate)))))

      (define (get-local-variable name)
        "Get the buffer-local variable NAME."
        (with-mutex bufstate-guard
          (xile-buffer-getvar bufstate name)))

      (define (set-local-variable! name value)
        "Set the buffer-local variable NAME to VALUE."
        (with-mutex bufstate-guard
          (xile-buffer-setvar! bufstate name value)))

      (define (cb-scroll-to scroll-to)
        "Callback to handle scroll_to message y x from Xi."
        (let ((line (xi-scroll-to-line scroll-to))
              (col (xi-scroll-to-col scroll-to)))
          (with-mutex bufwin-guard
            (set-xile-buffer-state-cursor bufstate (cons line col))
            (move (xile-buffer-state-bufwin bufstate) line col)
            (refresh (xile-buffer-state-bufwin bufstate)))))

      (define (cb-config-changed buffer-config-changes)
        "Callback to handle config_changed message BUFFER-CONFIG-CHANGE from Xi."
        (with-mutex bufstate-guard
          (xile-buffer-apply-config-change bufstate buffer-config-changes)))

      (define (cb-update result)
        "Callback to handle update message RESULT from Xi."
        (with-mutex bufstate-guard
          (set-xile-buffer-state-pristine bufstate (xi-update-pristine result))
          (set-xile-buffer-state-line_cache
           bufstate
           (xi-line-cache-execute-update (xile-buffer-state-line_cache bufstate) result)))
        (draw-buffer))

      (set! dispatch
        (lambda (m)
          (cond ((eq? m 'get-bufstate) bufstate)
                ((eq? m 'get-win) (xile-buffer-state-bufwin bufstate))
                ((eq? m 'get-var) get-local-variable)
                ((eq? m 'set-var!) set-local-variable!)
                ((eq? m 'create-view) create-view)
                ((eq? m 'scroll) scroll)
                ((eq? m 'scroll-view-up) scroll-view-up)
                ((eq? m 'scroll-view-down) scroll-view-down)
                ((eq? m 'move_up) (move_up))
                ((eq? m 'move_down) (move_down))
                ((eq? m 'cb-scroll-to) cb-scroll-to)
                ((eq? m 'cb-update) cb-update)
                ((eq? m 'cb-config-changed) cb-config-changed)
                (else (error (format #f "Unknown request : MAKE-XILE-BUFFER ~a~%" m))))))

      dispatch))
