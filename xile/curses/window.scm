;; coding: utf-8

(define-module (xile curses window)
  #:use-module (xile xi-protocol notification-types) ; For xi-line record
  #:use-module (xile xi-protocol line-cache)            ; For xi-line-cache record
  #:use-module (xile buffer state)          ; For xile-buffer-state record
  #:use-module (xile variables)             ; For global variables
  #:use-module (ncurses curses)             ; For the curses interface
  #:use-module (ice-9 threads)              ; For the with-mutex
  #:use-module (srfi srfi-43)               ; Vectors
  #:export (make-xile-footer
            update-footer
            draw-buffer-in-curses
            clear-footer-text
            make-xile-header
            update-header
            make-xile-main
            encode-key-to-string-sequence))

(define (encode-key-to-string-sequence ch)
  "Convert a key CH coming from `getch' to a string sequence.

Control-key is translated to [C-key] (brackets necessary for the keymaps as well).
Alt-key is translated to [M-key].
Actual [ is encoded as [[.
Special keys (like KEY_DOWN) or (KEY_PPAGE) are encoded as [DOWN] or [PPAGE].
Space is encoded as [SPC].

Since the keymap will just look for the concatenation of the keys in its hash-table, there is no
need to make a special treatment otherwise. The bracket format is only use to disambiguate
Control-M from hitting C then - then M."
  (cond
   ;; Escaped char
   ((equal? ch #\[)
    "[[")
   ;; Handled special keys
   ((equal? ch KEY_DOWN)
    "[DOWN]")
   ((equal? ch KEY_UP)
    "[UP]")
   ((equal? ch KEY_LEFT)
    "[LEFT]")
   ((equal? ch KEY_RIGHT)
    "[RIGHT]")
   ((equal? ch KEY_PPAGE)
    "[PPAGE]")
   ((equal? ch KEY_NPAGE)
    "[NPAGE]")
   ((equal? ch #\return)
    "[RET]")
   ((equal? ch #\space)
    "[SPC]")
   ;; Unhandled special keys
   ((not (char? ch))
    ch)
   ;; Alphas
   ((and (char<=? ch #\z) (char>=? ch #\a))
    (format #f "~a" ch))
   ((and (char<=? ch #\Z) (char>=? ch #\A))
    (format #f "~a" ch))
   ;; Unknown land
   (else
    ch)))

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
  (let* ((height (- (lines) footer-height header-height))
         (width 0)
         (startx 0)
         (starty header-height))
    (newwin height width starty startx)))

(define (draw-buffer-in-curses buffer window)
  "Draw the buffer BUFFER in WINDOW and refresh WINDOW (redisplay code)."
  (let* ((bufstate (buffer 'get-bufstate))
         (cache (xile-buffer-state-line_cache bufstate))
         (inv-before (xi-line-cache-invalid_before cache))
         (inv-after (xi-line-cache-invalid_after cache))
         (window-lines (getmaxy window))
         (current-view ((buffer 'current-view))))
    (format #t "Drawing between ~a and ~a~%" inv-before inv-after)
    (begin
      (vector-for-each
       (lambda (i line)
         (when (and (>= i inv-before) (< i inv-after))
           (if (and (xi-line-valid line) (>= (xi-line-ln line) (1+ (car current-view))))
               (addstr window
                       (format #f "~a" (xi-line-text line))
                       #:y (- (xi-line-ln line) (1+ (car current-view))) #:x 0))))
       (xi-line-cache-lines (xile-buffer-state-line_cache bufstate)))

      (when (xile-buffer-state-cursor bufstate)
        (move window
              (car (xile-buffer-state-cursor bufstate))
              (cdr (xile-buffer-state-cursor bufstate)))))

    ((buffer 'set-clean-display-state))
    (refresh window)))
