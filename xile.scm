#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(add-to-load-path (dirname (current-filename)))
(use-modules (ncurses curses)
             (ice-9 threads)            ;; For the join-thread in main
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
       (bkgdset! win (inverse #\sp))
       win)
     (newwin height width starty startx))))

(define (update-header header-win text)
      (addstr header-win text #:y 0 #:x 10)
      (refresh header-win))

(define (update-footer footer-win text)
  (addstr footer-win text #:y 0 #:x (round (/ (- (cols) (string-length text)) 2)))
  (refresh footer-win))

(define (make-xile-main)
  ;; TODO : Get proper height/width params
  (let* ((height (- (lines) 2 1))
         (width 0)
         (startx 0)
         (starty 1))
    (newwin height width starty startx)))

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

      ;; HACK Xi init code
      (xile-rpc-send port-to-xi (xile-msg-init '()))
      (let ((msg (xile-msg-new_view '((file_path . "README.org")))))
        (xile-register-callback
         (car msg)
         (lambda (result)
           ;; The output-port for this lambda is the xile-listen-err file-port
           (addstr xile-main
                   (format #f "Got ~a for the new_view message ~%~
                     -> Now I'd like to put the value back into ~
                        a parent environment binding~%" result)
                   #:y 10 #:x 0)
           (refresh xile-main)))
        (xile-rpc-send port-to-xi msg))

      ;; HACK Basic xile-main code
      (attr-set! xile-main A_NORMAL)
      (addstr xile-main "Type any character to see it in bold" #:y 8 #:x 30)
      (refresh xile-main)
      (let ((ch (getch xile-main)))
        (addstr xile-main "The pressed key is " #:y 9 #:x 30)
        (if (char? ch)
            (addch xile-main (bold ch))
            (addchstr xile-main (bold (keyname ch))))
        (refresh xile-main))

      ;; TODO : event loop thread instead of exiting just after this
      (getch xile-main))

    ;; Closing code (give back resources)
    (join-thread listener (current-time))
    (close-port port-to-xi)
    (close-port port-from-xi)
    (endwin)))
