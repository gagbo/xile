;; coding: utf-8

(define-module (xile curses-window)
  #:use-module (xile message)
  #:use-module (xile json-rpc)
  #:use-module (xile backend-notifications)
  #:use-module (xile line-cache)
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
            make-xile-main
            xile-buffer-info?
            xile-buffer-info-view_id
            set-xile-buffer-info-view_id
            xile-buffer-info-file_path
            set-xile-buffer-info-file_path
            xile-buffer-info-bufwin
            set-xile-buffer-info-bufwin))

(define (make-xile-header)
  "Return a window suitable for a Xile header."
  (let* ((height 1)
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
  (let* ((height 2)
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

(define-record-type <xile-buffer-info>
  (make-xile-buffer-info view_id file_path bufwin pristine line_cache cursor)
  xile-buffer-info?
  (view_id xile-buffer-info-view_id set-xile-buffer-info-view_id) ; string : internal Xi identifier for the buffer
  (file_path xile-buffer-info-file_path set-xile-buffer-info-file_path) ; string : file path to the file attached to the buffer
  (bufwin xile-buffer-info-bufwin set-xile-buffer-info-bufwin) ; ncurses window : window displaying the buffer
  (pristine xile-buffer-info-pristine set-xile-buffer-info-pristine) ; boolean : pristine (unsaved) state
  (line_cache xile-buffer-info-line_cache set-xile-buffer-info-line_cache) ; xi-line-cache : line cache for the buffer
  (cursor xile-buffer-info-cursor set-xile-buffer-info-cursor)) ; (int . int . nil) :  Cursor position as (y . x . nil)

(define make-xile-buffer #f)
(define find-xile-buffer #f)

(let
    ((id-to-buffer (make-hash-table 31))
     (id-to-buffer-guard (make-mutex)))

  (set! find-xile-buffer
    (lambda (view_id)
      "Find a buffer from VIEW_ID. Return #f if VIEW_ID is not registered yet."
      (with-mutex id-to-buffer-guard
        (hashq-ref id-to-buffer view_id))))

  (set! make-xile-buffer
    (lambda* (port-to-xi send-mutex #:optional file_path)
      "Create a buffer with PORT-TO-XI used to send messages to xi-core process.
SEND-MUTEX protects access to PORT-TO-XI

Optional FILE_PATH can be given to set the view to point to a file.

The dispatching of the returned lambda can be checked in source code.
- cb* messages/procedures regard callbacks to back-end notifications
- Other messages/procedures usually wrap message sending to xi
- A few accessors for the variables in the closures are added

Opening multiple buffers pointing to the same FILE_PATH is undefined behaviour,
as of 2020-03-09, xi doesn't handle multiple views of a single file."
      (let* ((info (make-xile-buffer-info
                    #f file_path (make-xile-main) #t (make-xi-line-cache #() 0 0) #f))
             (current-view (cons 0 (getmaxy (xile-buffer-info-bufwin info))))
             (to-xi port-to-xi)
             (to-xi-guard send-mutex)
             (bufwin-guard (make-mutex))
             (info-guard (make-mutex)))

        (define dispatch #f)          ; dispatch acts as "this" in OOP-languages

        ;; TODO : Refactor the notification sending out of buffer.
        ;; This is the most stateless part we can start with.
        (define (rpc-send-notif notif)
          "Send the notification NOTIF to Xi through RPC."
          (xile-rpc-send to-xi to-xi-guard notif))

        (define (scroll min-line max-line)
          "Send Scroll[MIN-LINE MAX-LINE]."
          (rpc-send-notif (xile-msg-edit-scroll (xile-buffer-info-view_id info) min-line max-line)))

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
          (rpc-send-notif (xile-msg-edit-move_down (xile-buffer-info-view_id info))))

        (define (move_up)
          "Move the cursor up."
          (rpc-send-notif (xile-msg-edit-move_up (xile-buffer-info-view_id info))))

        (define (create-view)
          " \"\" Constructor \"\" => sends a \"new_view\" message and sets the attribute in the callback. "
          (keypad! (xile-buffer-info-bufwin info) #t)
          (curs-set 1)
          (let ((msg (xile-msg-new_view #:file_path file_path))
                (wait-for-id (make-condition-variable))
                (guard-wait (make-mutex)))
            (xile-register-callback
             (car msg)
             (lambda (result)
               (lock-mutex id-to-buffer-guard)
               (when (and (xile-buffer-info-view_id info) (hashq-get-handle id-to-buffer (xile-buffer-info-view_id info)))
                 hashq-remove! id-to-buffer (xile-buffer-info-view_id info))
               (hashq-set! id-to-buffer (string->symbol result) dispatch)
               (unlock-mutex id-to-buffer-guard)
               (set-xile-buffer-info-view_id info result)
               (signal-condition-variable wait-for-id)))
            ;; HACK : using a condvar here means we need to lock/unlock a mutex.
            ;; Seems weird
            (xile-rpc-send to-xi to-xi-guard msg)
            (with-mutex id-to-buffer-guard
              (wait-condition-variable wait-for-id id-to-buffer-guard))))

        (define (draw-buffer)
          "Draw the buffer in its window and refresh the window (redisplay code)."
          (with-mutex bufwin-guard
            (let* ((cache (xile-buffer-info-line_cache info))
                   (inv-before (xi-line-cache-invalid_before cache))
                   (inv-after (xi-line-cache-invalid_after cache))
                   (window-lines (getmaxy (xile-buffer-info-bufwin info))))
              (format (current-output-port) "Drawing between ~a and ~a~%" inv-before inv-after)
              (begin
                (vector-for-each
                 (lambda (i line)
                   (when (and (>= i inv-before) (< i inv-after))
                     (if (and (xi-line-valid line) (>= (xi-line-ln line) (1+ (car current-view))))
                         (addstr (xile-buffer-info-bufwin info)
                                 (format #f "~a" (xi-line-text line))
                                 #:y (- (xi-line-ln line) (1+ (car current-view))) #:x 0))))
                 (xi-line-cache-lines (xile-buffer-info-line_cache info)))

                (when (xile-buffer-info-cursor info)
                  (move (xile-buffer-info-bufwin info)
                        (car (xile-buffer-info-cursor info))
                        (cadr (xile-buffer-info-cursor info)))))

              (refresh (xile-buffer-info-bufwin info)))))

        (define (cb-scroll-to scroll-to)
          "Callback to handle scroll_to message y x from Xi."
          (with-mutex bufwin-guard
            (move (xile-buffer-info-bufwin info) (xi-scroll-to-line scroll-to) (xi-scroll-to-col scroll-to))
            (refresh (xile-buffer-info-bufwin info))))

        (define (cb-update result)
          "Callback to handle update message RESULT from Xi."
          (with-mutex info-guard
            (set-xile-buffer-info-pristine info (xi-update-pristine result))
            (set-xile-buffer-info-line_cache
             info
             (xi-line-cache-execute-update (xile-buffer-info-line_cache info) result)))
          (draw-buffer))

        (set! dispatch
          (lambda (m)
            (cond ((eq? m 'get-info) info)
                  ((eq? m 'get-win) (xile-buffer-info-bufwin info))
                  ((eq? m 'create-view) create-view)
                  ((eq? m 'scroll) scroll)
                  ((eq? m 'scroll-view-up) scroll-view-up)
                  ((eq? m 'scroll-view-down) scroll-view-down)
                  ((eq? m 'move_up) (move_up))
                  ((eq? m 'move_down) (move_down))
                  ((eq? m 'cb-scroll-to) cb-scroll-to)
                  ((eq? m 'cb-update) cb-update)
                  (else (error (format #f "Unknown request : MAKE-XILE-BUFFER ~a~%" m))))))

        dispatch))))
