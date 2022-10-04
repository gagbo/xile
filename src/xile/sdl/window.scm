;; coding: utf-8

(define-module (xile sdl window)
  #:use-module (sdl2)
  #:use-module (sdl2 events)
  #:use-module (sdl2 rect)
  #:use-module (sdl2 render)
  #:use-module (sdl2 surface)
  #:use-module (sdl2 ttf)
  #:use-module (sdl2 video)
  #:use-module (xile xi-protocol notification-types) ; For xi-line record
  #:use-module (xile xi-protocol line-cache)            ; For xi-line-cache record
  #:use-module (xile buffer state)          ; For xile-buffer-state record
  #:use-module (xile variables)             ; For global variables
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)              ; For the with-mutex
  #:use-module (srfi srfi-1)                ; Find
  #:use-module (srfi srfi-43)               ; Vectors
  #:export (make-xile-window
            draw-buffer-in-sdl
            xile-sdl-font
            grid-size
            grid-unit-size))

(define (make-xile-window)
  "Return a window suitable to hold Xile."
  (make-window #:title "Xile editor - Guile Scheme goes with everything"))

(define (xile-sdl-font) (load-font font-path 16))

(define (grid-unit-size font)
  "Return the grid size in pixel pair (width . height) for the given FONT.

This is better used with a monospace font,
as only a few characters are sampled to guess the grid size to use."
  (let ((line-skip (font-line-skip font)))
    (receive (minx miny maxx maxy advance)
        (font-glyph-metrics font #\f)
      (cons advance line-skip))))

(define (grid-size window font)
  "Return the size of the text grid in WINDOW using given FONT.

The result is given as a pair (grid-width . grid-height)"
  (let ((grid-unit (grid-unit-size font)))
    (receive (winW winH)
        (window-size window)
      (cons (quotient winW (car grid-unit))
            (quotient winH (cdr grid-unit))))))

(define* (grid-to-px font #:key (y 0) (x 0))
  "Return the pixel positions to use for position (X, Y) in the grid using FONT.

The result has the form (pixel_y . pixel_x).

The convention is always (0, 0) in the top left corner."
  (let* ((grid-unit (grid-unit-size font))
         (grid-unit-height (cdr grid-unit))
         (grid-unit-width (car grid-unit)))
    (cons (* y grid-unit-height) (* x grid-unit-width))))

(define (draw-char-in-sdl char font dest-surface grid-offset)
  "Draw the character CHAR using FONT in DEST-SURFACE with horizontal GRID-OFFSET.

GRID-OFFSET is a grid position (grid-x)"
  (let* ((surface (render-font-blended font (make-string 1 char) (make-color #x30 #xAA #xE8 #x00)))
         (origin-px (grid-to-px font #:y 0 #:x grid-offset))
         (grid-unit (grid-unit-size font))
         (dest-rect (make-rect (cdr origin-px) 0 (car grid-unit) (cdr grid-unit))))
    (blit-surface surface #f dest-surface dest-rect)))

(define* (draw-line-in-sdl line font dest-surface #:key (start-grid-y 0) (start-grid-x 0))
  "Draw the line LINE using FONT in DEST-SURFACE at (START-GRID-Y . START-GRID-X)."
  (let* ((grid-unit (grid-unit-size font))
         (line-surface (make-rgb-surface (surface-width dest-surface) (cdr grid-unit) 24))
         (origin-px (grid-to-px font #:y start-grid-y #:x start-grid-x))
         (dest-rect (make-rect (cdr origin-px) (car origin-px) (surface-width dest-surface) (cdr grid-unit))))
    (vector-for-each
     (lambda (i char)
       (unless (eq? char #\newline)     ; Skip the end newline character
         (draw-char-in-sdl char font line-surface i)))
     (list->vector (string->list (xi-line-text line))))
    (blit-surface line-surface #f dest-surface dest-rect)))

;; FIXME : Find out why sometimes the draw-buffer procedure throws and prevents refreshing
;; This below appears in xile-listen-err.log
;; In thread:
;; In procedure load-font: Error while printing exception.
(define (draw-buffer-in-sdl buffer width height renderer)
  "Draw the buffer BUFFER in RENDERER and refresh RENDERER (redisplay code)."
  (let* ((bufstate (buffer 'get-bufstate))
         (cache (xile-buffer-state-line_cache bufstate))
         (inv-before (xi-line-cache-invalid_before cache))
         (inv-after (xi-line-cache-invalid_after cache))
         (current-view ((buffer 'current-view)))
         (text-surface (make-rgb-surface width height 24)))
    (format #t "Drawing between ~a and ~a~%" inv-before inv-after)
    (clear-renderer renderer)
    (begin
      (vector-for-each
       (lambda (i line)
         (when (and (>= i inv-before) (< i inv-after))
           (if (and (xi-line-valid line) (>= (xi-line-ln line) (1+ (car current-view))))
               (draw-line-in-sdl line (xile-sdl-font) text-surface #:start-grid-y (- (xi-line-ln line) (1+ (car current-view))) #:start-grid-x 0))))
       (xi-line-cache-lines (xile-buffer-state-line_cache bufstate)))

      (when (xile-buffer-state-cursor bufstate)
        ;; FIXME : make a draw-cursor procedure
        ;; FIXME : move cursor at the correct position
        #f))

    ((buffer 'set-clean-display-state))
    (render-copy renderer (surface->texture renderer text-surface))
    (present-renderer renderer)))
