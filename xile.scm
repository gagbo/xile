#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (ncurses curses)
             (ice-9 threads)            ;; For the join-thread in main
             (ice-9 futures)
             (xile message)
             (xile json-rpc))

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

(define make-buffer #f)
(define find-buffer #f)
(let 
    ((id-to-buffer (make-hash-table 31))
     (id-to-buffer-guard (make-mutex)))

  (set! find-buffer
    (lambda (view_id)
      (with-mutex id-to-buffer-guard
        (hashq-ref id-to-buffer view_id))))

  (set! make-buffer
    (lambda* (port-to-xi #:optional file_path)
      (let ((view_id #f)                ; The internal identifier for xi
            (bufwin (make-xile-main))   ; The associated ncurses window/panel
            (file_path file_path)
            (to-xi port-to-xi))

        (define dispatch #f)          ; dispatch acts as "this" in OOP-languages

        (define (create-view) ; "" Constructor "" => sends a "new_view" message and sets the attribute in the callback
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
                  (else (error (format #f "Unknown request : MAKE-BUFFER ~a~%" m))))))

        dispatch))))

;; Main
(define (main args)
  ;; This let initializes resources
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (port-to-xi (cddr xi-setup))
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
          (xile-main (make-xile-main))
          (xile-footer (make-xile-footer)))

      (keypad! xile-header #t)
      (keypad! xile-main #t)
      (keypad! xile-footer #t)

      ;; Header and Footer init
      (update-header xile-header "Xile alpha -- header")
      (update-footer xile-footer "Xile alpha -- footer")

      ;; HACK Logging init code
      (set-current-error-port (open "logs/xile-err.log" (logior O_APPEND O_CREAT O_WRONLY)))
      (set-current-output-port (open "logs/xile-out.log" (logior O_APPEND O_CREAT O_WRONLY)))

      ;; Xi init code
      (xile-rpc-send port-to-xi (xile-msg-init))

      (begin
        ;; HACK register notification callback
        (xile-register-callback
         'update
         ;; The "update" callback here is just badly extracting the text
         ;; from the first line of updates.
         (lambda (result)
           (let* ((view_id (assoc-ref result "view_id"))
                  (xile-buffer (find-buffer (string->symbol view_id))))
             (format #t "Update : xile-buffer for ~a is ~a~%" view_id xile-buffer)
             (when xile-buffer
               ((xile-buffer 'cb-update) result)))))

        (xile-register-callback
         'scroll_to
         (lambda (result)
           (let* ((y (assoc-ref result "line"))
                 (x (assoc-ref result "col"))
                 (view_id (assoc-ref result "view_id"))
                 (xile-buffer (find-buffer (string->symbol view_id))))
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
        (define first-buffer (make-buffer port-to-xi "README.org"))
        ((first-buffer 'create-view))
        ((first-buffer 'scroll) 0 (- (lines) 3))
        )



      ;; Main event loop
      (let loop ((ch (getch xile-main)))
        (cond
         ((eqv? ch #\q)
          #f)

         (else
          (clear-footer-text xile-footer)
          (addstr xile-footer (format #f "Press q to quit (you pressed ~a)" ch) #:y 1 #:x 0)
          (refresh xile-footer)
          (loop (getch xile-main))))))

    ;; Closing code (give back resources)
    (join-thread listener (current-time))
    (close-port port-to-xi)
    (close-port port-from-xi)
    (endwin)))
