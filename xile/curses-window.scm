;; coding: utf-8

(define-module (xile curses-window)
  #:use-module (xile message)
  #:use-module (xile json-rpc)
  #:use-module (xile backend-notifications)
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
  (let* ((height 2)
         (width 0)
         (startx 0)
         (starty (- (lines) height)))
    ((lambda (win)
       (attr-set! win A_REVERSE)
       win)
     (newwin height width starty startx))))

(define (update-header header-win text)
  ;; TODO Make the refresh optional
  (addstr header-win text #:y 0 #:x 10)
  (refresh header-win))

(define (update-footer footer-win text)
  ;; TODO Make the refresh optional

  ;; HACK : bkdgset! scope assumes the old value is (normal #\sp) and hardcodes it in the
  ;; function.
  ;; It would be better if the value was captured in a let-binding
  (bkgdset! footer-win (inverse #\sp))
  (addstr footer-win text #:y 0 #:x (round (/ (- (cols) (string-length text)) 2)))
  (bkgdset! footer-win (normal #\sp))
  (refresh footer-win))

(define (clear-footer-text footer-win)
  ;; TODO Make the refresh optional
  (move footer-win 1 0)
  (clrtobot footer-win)
  (refresh footer-win))

(define (make-xile-main)
  ;; TODO : Get proper height/width params
  ;; 2 is (getmaxy xile-footer-win) and 1 is (getmaxy xile-header-win)
  ;; But HOW can I get these into scope properly for evaluation ?
  (let* ((height (- (lines) 2 1))
         (width 0)
         (startx 0)
         (starty 1))
    (newwin height width starty startx)))

(define-record-type <xi-line-cache>
  (make-xi-line-cache lines invalid_before invalid_after)
  xi-line-cache?
  (lines xi-line-cache-lines set-xi-line-cache-lines)
  (invalid_before xi-line-cache-invalid_before set-xi-line-cache-invalid_before)
  (invalid_after xi-line-cache-invalid_after set-xi-line-cache-invalid_after))

(define (xi-line-cache-handle-update-update cache update)
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
        (old-lines (xi-line-cache-lines cache))
        (count (xi-op-count update)))
    (make-xi-line-cache old-lines inv-before inv-after)))

(define (xi-line-cache-handle-update-copy cache update)
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
        (old-lines (xi-line-cache-lines cache))
        (count (xi-op-count update)))

    ;; Do not do anything if we're asked to copy everything
    (if (and (= 0 inv-before) (= count (vector-length old-lines)))
        cache
        (let* ((new-lines (make-vector (- inv-after inv-before))))
          (vector-copy! new-lines 0 old-lines (+ inv-before count) inv-after)
          (vector-copy! new-lines (- inv-after (+ inv-before count)) old-lines inv-before (+ inv-before count))
          (make-xi-line-cache new-lines 0 (- inv-after inv-before))))))

(define (xi-line-cache-handle-update-skip cache update)
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
        (old-lines (xi-line-cache-lines cache))
        (count (xi-op-count update)))

    (define new-lines (vector-copy old-lines (+ count inv-before)))
    (make-xi-line-cache new-lines 0 (- inv-after count))))

(define (xi-line-cache-handle-update-invalidate cache update)
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
        (old-lines (xi-line-cache-lines cache))
        (count (xi-op-count update)))

    (define new-lines (make-vector (+ (vector-length old-lines) count)))
    (vector-copy! new-lines 0 old-lines)

    (if (equal? (vector-length old-lines) 0)
        (make-xi-line-cache new-lines (+ inv-before count) inv-after)
        (make-xi-line-cache new-lines inv-before inv-after))))

(define (xi-line-cache-handle-update-ins cache update)
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
        (old-lines (xi-line-cache-lines cache))
        (count (xi-op-count update))
        (ins-lines (xi-op-lines update)))

    (define new-lines (make-vector (+ inv-after (vector-length ins-lines))))
    (vector-copy! new-lines 0 old-lines 0 inv-after)
    (vector-copy! new-lines inv-after ins-lines)

    (make-xi-line-cache new-lines inv-before (vector-length new-lines))))

(define-record-type <xile-buffer-info>
  (make-xile-buffer-info view_id file_path bufwin pristine line_cache index cursor)
  xile-buffer-info?
  (view_id xile-buffer-info-view_id set-xile-buffer-info-view_id)
  (file_path xile-buffer-info-file_path set-xile-buffer-info-file_path)
  (bufwin xile-buffer-info-bufwin set-xile-buffer-info-bufwin)
  (pristine xile-buffer-info-pristine set-xile-buffer-info-pristine)
  (line_cache xile-buffer-info-line_cache set-xile-buffer-info-line_cache)
  (index xile-buffer-info-index set-xile-buffer-info-index)
  (cursor xile-buffer-info-cursor set-xile-buffer-info-cursor))

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
                    #f file_path (make-xile-main) #t (make-xi-line-cache #() 0 0) 0 #f))
             (to-xi port-to-xi)
             (to-xi-guard send-mutex)
             (bufwin-guard (make-mutex))
             (info-guard (make-mutex)))

        (define dispatch #f)          ; dispatch acts as "this" in OOP-languages

        ;; TODO : Refactor the notification sending out of buffer.
        ;; This is the most stateless part we can start with.
        (define (rpc-send-notif notif)
          (xile-rpc-send to-xi to-xi-guard notif))

        (define (scroll min-line max-line)
          (rpc-send-notif (xile-msg-edit-scroll (xile-buffer-info-view_id info) min-line max-line)))

        (define (move_down)
          (rpc-send-notif (xile-msg-edit-move_down (xile-buffer-info-view_id info))))

        (define (move_up)
          (rpc-send-notif (xile-msg-edit-move_up (xile-buffer-info-view_id info))))

        (define (create-view) ; "" Constructor "" => sends a "new_view" message and sets the attribute in the callback
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
          (with-mutex bufwin-guard
            (let* ((index (xile-buffer-info-index info))
                  (cache (xile-buffer-info-line_cache info))
                  (inv-before (xi-line-cache-invalid_before cache))
                  (inv-after (xi-line-cache-invalid_after cache))
                  (window-lines (getmaxy (xile-buffer-info-bufwin info))))
              (format (current-output-port) "Drawing between ~a and ~a~%" inv-before inv-after)
              (begin
                (vector-for-each
                 (lambda (i line)
                   (when (and (>= i inv-before) (< i inv-after))
                     (if (xi-line-valid line)
                         (addstr (xile-buffer-info-bufwin info)
                                 (format #f "~a" (xi-line-text line))
                                 #:y (- i index) #:x 0))))
                 (xi-line-cache-lines (xile-buffer-info-line_cache info)))

                (when (xile-buffer-info-cursor info)
                  (move (xile-buffer-info-bufwin info)
                        (car (xile-buffer-info-cursor info))
                        (cadr (xile-buffer-info-cursor info)))))

              (refresh (xile-buffer-info-bufwin info)))))

        (define (cb-scroll-to y x)
          (with-mutex bufwin-guard
            (move (xile-buffer-info-bufwin info) y x)
            (refresh (xile-buffer-info-bufwin info))))

        (define (cb-update result)
          ;; The "update" callback here is just badly extracting the text
          ;; from the first line of updates.
          (with-mutex info-guard
            (set-xile-buffer-info-pristine info (xi-update-pristine result))
            (and=>
             (xi-update-ops result)
             (lambda (vec)
               (vector-for-each
                (lambda (i op)
                  (handle-update-op op))
                vec))))
          (draw-buffer))

        (define (handle-update-op op)
          (let ((type (xi-op-type op))
                (cache (xile-buffer-info-line_cache info)))
            (cond
             ((eq? type 'ins)
              (set-xile-buffer-info-line_cache info
                                               (xi-line-cache-handle-update-ins cache op))
              (format (current-error-port) "After ins : ~%~y~%" (xile-buffer-info-line_cache info)))
             ((eq? type 'invalidate)
              (set-xile-buffer-info-line_cache info
                                               (xi-line-cache-handle-update-invalidate cache op))
              (format (current-error-port) "After invalidate : ~%~y~%" (xile-buffer-info-line_cache info)))
             ((eq? type 'copy)
              (set-xile-buffer-info-line_cache info
                                               (xi-line-cache-handle-update-copy cache op))
              (format (current-error-port) "After copy : ~%~y~%" (xile-buffer-info-line_cache info)))
             ((eq? type 'skip)
              (set-xile-buffer-info-line_cache info
                                               (xi-line-cache-handle-update-skip cache op))
              (format (current-error-port) "After skip : ~%~y~%" (xile-buffer-info-line_cache info)))
             ((eq? type 'update)
              (set-xile-buffer-info-line_cache info
                                               (xi-line-cache-handle-update-update cache op))
              (format (current-error-port) "After update : ~%~y~%" (xile-buffer-info-line_cache info)))
             (else (format (current-error-port) "Unknown update type : ~a~%" type)))))

        (set! dispatch
          (lambda (m)
            (cond ((eq? m 'get-info) info)
                  ((eq? m 'get-win) (xile-buffer-info-bufwin info))
                  ((eq? m 'create-view) create-view)
                  ((eq? m 'scroll) scroll)
                  ((eq? m 'move_up) (move_up))
                  ((eq? m 'move_down) (move_down))
                  ((eq? m 'cb-scroll-to) cb-scroll-to)
                  ((eq? m 'cb-update) cb-update)
                  (else (error (format #f "Unknown request : MAKE-XILE-BUFFER ~a~%" m))))))

        dispatch))))
