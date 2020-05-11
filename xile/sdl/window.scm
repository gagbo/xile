;; coding: utf-8

(define-module (xile sdl window)
  #:use-module (xile xi-protocol notification-types) ; For xi-line record
  #:use-module (xile xi-protocol line-cache)            ; For xi-line-cache record
  #:use-module (xile buffer state)          ; For xile-buffer-state record
  #:use-module (xile variables)             ; For global variables
  #:use-module (ice-9 threads)              ; For the with-mutex
  #:use-module (srfi srfi-43)               ; Vectors
  #:export (encode-key-to-string-sequence
            make-xile-window
            draw-buffer-in-sdl))

(define (encode-key-to-string-sequence ch)
  "Convert a key CH coming from SDL event to a string sequence.

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

(define (make-xile-window)
  "Return a window suitable to hold Xile."
  #f)

(define (draw-buffer-in-sdl buffer window)
  "Draw the buffer BUFFER in WINDOW and refresh WINDOW (redisplay code)."
  (let* ((bufstate (buffer 'get-bufstate))
         (cache (xile-buffer-state-line_cache bufstate))
         (inv-before (xi-line-cache-invalid_before cache))
         (inv-after (xi-line-cache-invalid_after cache))
         (current-view ((buffer 'current-view))))
    (format #t "Drawing between ~a and ~a~%" inv-before inv-after)
    (begin
      (vector-for-each
       (lambda (i line)
         (when (and (>= i inv-before) (< i inv-after))
           (if (and (xi-line-valid line) (>= (xi-line-ln line) (1+ (car current-view))))
               ;; TODO : Draw line in the window
               #f)))
       (xi-line-cache-lines (xile-buffer-state-line_cache bufstate)))

      (when (xile-buffer-state-cursor bufstate)
        ;; TODO : move cursor at the correct position
        #f))

    ((buffer 'set-clean-display-state))
    ;; (refresh window)
    ))
