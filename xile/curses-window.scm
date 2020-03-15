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
  (let* ((height (- (lines) 2 1))
         (width 0)
         (startx 0)
         (starty 1))
    (newwin height width starty startx)))

(define-record-type <xile-buffer-info>
  (make-xile-buffer-info view_id file_path bufwin pristine lines index)
  xile-buffer-info?
  (view_id xile-buffer-info-view_id set-xile-buffer-info-view_id)
  (file_path xile-buffer-info-file_path set-xile-buffer-info-file_path)
  (bufwin xile-buffer-info-bufwin set-xile-buffer-info-bufwin)
  (pristine xile-buffer-info-pristine set-xile-buffer-info-pristine)
  (lines xile-buffer-info-lines set-xile-buffer-info-lines)
  (index xile-buffer-info-index set-xile-buffer-info-index))

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
      (let* ((view_id #f)               ; The internal identifier for xi
             (bufwin (make-xile-main))  ; The associated ncurses window/panel
             (file_path file_path)
             (pristine #t)
             (lines #())
             (index 0)
             (info (make-xile-buffer-info view_id file_path bufwin pristine lines index))
             (to-xi port-to-xi)
             (to-xi-guard send-mutex)
             (bufwin-guard (make-mutex)))

        (define dispatch #f)          ; dispatch acts as "this" in OOP-languages

        (define (create-view) ; "" Constructor "" => sends a "new_view" message and sets the attribute in the callback
          (keypad! bufwin #t)
          (let ((msg (xile-msg-new_view #:file_path file_path))
                (wait-for-id (make-condition-variable))
                (guard-wait (make-mutex)))
            (xile-register-callback
             (car msg)
             (lambda (result)
               (lock-mutex id-to-buffer-guard)
               (when (and view_id (hashq-get-handle id-to-buffer view_id))
                 hashq-remove! id-to-buffer view_id)
               (hashq-set! id-to-buffer (string->symbol result) dispatch)
               (unlock-mutex id-to-buffer-guard)
               (set! view_id result)
               (signal-condition-variable wait-for-id)))
            ;; HACK : using a condvar here means we need to lock/unlock a mutex.
            ;; Seems weird
            (xile-rpc-send to-xi to-xi-guard msg)
            (with-mutex id-to-buffer-guard
              (wait-condition-variable wait-for-id id-to-buffer-guard))))

        (define (scroll min-line max-line)
          (xile-rpc-send to-xi to-xi-guard (xile-msg-edit-scroll view_id min-line max-line)))

        (define (draw-buffer)
          (with-mutex bufwin-guard
            (addstr bufwin
                    (format #f "~a" (xi-line-text (vector-ref (xile-buffer-info-lines info) 0)))
                    #:y 0 #:x 0)
            (refresh bufwin)))

        (define (cb-scroll-to y x)
          (with-mutex bufwin-guard
            (move bufwin y x)
            (refresh bufwin)))

        (define (cb-update result)
          ;; The "update" callback here is just badly extracting the text
          ;; from the first line of updates.
          (set-xile-buffer-info-pristine info (xi-update-pristine result))
          (and=> (xi-update-ops result) (lambda (vec) (vector-for-each (lambda (i op) (handle-update-op op)) vec)))
          (draw-buffer))

        (define (handle-update-op-ins op)
          (let* ((count (xi-op-count op))
                 (old_lines (xile-buffer-info-lines info))
                 (old_lines_len (vector-length old_lines))
                 (ins_lines (xi-op-lines op)))
            (define new_lines (make-vector (+ count old_lines_len)))

            (vector-for-each
             (lambda (i line)
               (if (< i old_lines_len)
                   (vector-set! new_lines i (vector-ref old_lines i))
                   (vector-set! new_lines i (vector-ref ins_lines (- i old_lines_len)))))
             new_lines)

            (set-xile-buffer-info-lines info new_lines))
          )

        (define (handle-update-op-copy op)
          #f)

        (define (handle-update-op-skip op)
          (let ((old_ix (xile-buffer-info-index info))
                (count (xi-op-count op)))
            (set-xile-buffer-info-index info (+ old_ix count))))

        (define (handle-update-op-invalidate op)
          ;; Use (set-xi-line-valid line #f) or (make-xi-line #f #f #f #f #f)
          ;; (xi-op-count op) times
          #f)

        (define (handle-update-op-update op)
          ;; Not handled by protocol yet
          (let ((old_ix (xile-buffer-info-index info))
                (count (xi-op-count op)))
            (set-xile-buffer-info-index info (+ old_ix count))))


        (define (handle-update-op op)
          (let ((type (xi-op-type op)))
            (cond ((eq? type 'ins)
                   (handle-update-op-ins op))
                  ((eq? type 'copy)
                   (handle-update-op-copy op))
                  ((eq? type 'skip)
                   (handle-update-op-skip op))
                  ((eq? type 'invalidate)
                   (handle-update-op-invalidate op))
                  ((eq? type 'update)
                   (handle-update-op-update op))
                  (else (format (current-error-port) "Unknown update type : ~a~%" type)))))

        (set! dispatch
          (lambda (m)
            (cond ((eq? m 'get-info) info)
                  ((eq? m 'get-win) (xile-buffer-info-bufwin info))
                  ((eq? m 'create-view) create-view)
                  ((eq? m 'scroll) scroll)
                  ((eq? m 'cb-scroll-to) cb-scroll-to)
                  ((eq? m 'cb-update) cb-update)
                  (else (error (format #f "Unknown request : MAKE-XILE-BUFFER ~a~%" m))))))

        dispatch))))
