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

      ;; Xi init code
      (xile-rpc-send port-to-xi (xile-msg-init '()))

      (begin
        ;; HACK register notification callback
        (xile-register-callback
         'update
         ;; The "update" callback here is just badly extracting the text
         ;; from the first line of updates.
         (lambda (result)
           (addstr xile-main
                   (format #f "~a" (assoc-ref (vector-ref (assoc-ref (vector-ref (assoc-ref (assoc-ref result "update") "ops") 0) "lines") 0) "text"))
                   #:y 0 #:x 0)
           (refresh xile-main)))

        (xile-register-callback
         'scroll_to
         (lambda (result)
           (let ((y (assoc-ref result "line"))
                 (x (assoc-ref result "col")))
             (move xile-main y x))
           (refresh xile-main)))

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

        ;; HACK open file
        (let ((msg (xile-msg-new_view '((file_path . "README.org")))))
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
             (refresh xile-main)))
          (xile-rpc-send port-to-xi msg)))

      ;; Main event loop
      (let loop ((ch (getch xile-main)))
        (cond
         ((eqv? ch #\q)
          #f)

         (else
          ;; FIXME : try hitting F-key and then a character key
          ;; Expected : the line shown has no character after closing parens
          ;; Actual : residual of the longer string before the char-key
          ;;          (because F-keys are 3 chars longs in their representation)
          (addstr xile-footer (format #f "Press q to quit (you pressed ~a)" ch) #:y 1 #:x 0)
          (refresh xile-footer)
          (loop (getch xile-main))))))

    ;; Closing code (give back resources)
    (join-thread listener (current-time))
    (close-port port-to-xi)
    (close-port port-from-xi)
    (endwin)))
