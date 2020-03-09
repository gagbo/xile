;; coding: utf-8

(define-module (xile curses-window)
  #:use-module (xile message)
  #:use-module (xile json-rpc)
  #:use-module (ncurses curses)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 threads)
  #:export (make-xile-buffer
            find-xile-buffer
            make-xile-footer
            update-footer
            clear-footer-text
            make-xile-header
            update-header
            make-xile-main))

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

(define make-xile-buffer (lambda* (port #:optional path) (error "Make buffer is defined but not properly set")))
(define find-xile-buffer #f)

(let
    ((id-to-buffer (make-hash-table 31))
     (id-to-buffer-guard (make-mutex)))

  (set! find-xile-buffer
    (lambda (view_id)
      (with-mutex id-to-buffer-guard
        (hashq-ref id-to-buffer view_id))))

  (set! make-xile-buffer
    (lambda* (port-to-xi #:optional file_path)
      (let ((view_id #f)                ; The internal identifier for xi
            (bufwin (make-xile-main))   ; The associated ncurses window/panel
            (file_path file_path)
            (to-xi port-to-xi))

        (define dispatch #f)          ; dispatch acts as "this" in OOP-languages

        (define (create-view) ; "" Constructor "" => sends a "new_view" message and sets the attribute in the callback
          (keypad! bufwin #t)
          (let ((msg (xile-msg-new_view #:file_path file_path))
                (wait-for-id (make-condition-variable))
                (guard-wait (make-mutex)))
            (xile-register-callback
             (car msg)
             (lambda (result)
               ;; TODO : Make a real structure around xile-main to hold some state
               ;;
               ;; I want to go with the one window = one view pattern
               ;; (eventually using panels to superimpose different views between the header and the footer)
               ;; Therefore, the xile-main window needs to be associated with the "view_id" returned somehow
               ;;
               ;; An application level (view_id: symbol -> Panel/window struct) hash map will be implemented.
               ;; This map will have :
               ;; - some writes in this callback, and
               ;; - some reads in basically all back_end notifications callbacks
               (lock-mutex id-to-buffer-guard)
               (when (and view_id (hashq-get-handle id-to-buffer view_id))
                 hashq-remove! id-to-buffer view_id)
               (hashq-set! id-to-buffer (string->symbol result) dispatch)
               (unlock-mutex id-to-buffer-guard)
               (set! view_id result)
               (signal-condition-variable wait-for-id)))
            ;; HACK : using a condvar here means we need to lock/unlock a mutex.
            ;; Seems weird
            (xile-rpc-send to-xi msg)
            (with-mutex id-to-buffer-guard
              (wait-condition-variable wait-for-id id-to-buffer-guard))))

        (define (scroll min-line max-line)
          (xile-rpc-send to-xi (xile-msg-edit-scroll view_id min-line max-line)))

        (define (cb-scroll-to y x)
          (move bufwin y x)
          (refresh bufwin))

        (define (cb-update result)
          (addstr bufwin
                  (format #f "~a" (assoc-ref (vector-ref (assoc-ref (vector-ref (assoc-ref (assoc-ref result "update") "ops") 0) "lines") 0) "text"))
                  #:y 0 #:x 0)
          (refresh bufwin))

        (set! dispatch
          (lambda (m)
            (cond ((eq? m 'get-view_id) view_id)
                  ((eq? m 'get-win) bufwin)
                  ((eq? m 'get-file_path) file_path)
                  ((eq? m 'create-view) create-view)
                  ((eq? m 'scroll) scroll)
                  ((eq? m 'cb-scroll-to) cb-scroll-to)
                  ((eq? m 'cb-update) cb-update)
                  (else (error (format #f "Unknown request : MAKE-XILE-BUFFER ~a~%" m))))))

        dispatch))))
