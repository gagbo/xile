;; coding: utf-8
;; Xile buffer state and variables

(define-module (xile buffer-state)
  #:use-module (srfi srfi-9)
  #:export (make-xile-buffer-state
            xile-buffer-setvar!
            xile-buffer-getvar
            xile-buffer-state?
            xile-buffer-state-view_id
            set-xile-buffer-state-view_id
            xile-buffer-state-file_path
            set-xile-buffer-state-file_path
            xile-buffer-state-line_cache
            set-xile-buffer-state-line_cache
            xile-buffer-state-cursor
            set-xile-buffer-state-cursor
            xile-buffer-state-bufwin
            set-xile-buffer-state-bufwin
            set-xile-buffer-state-pristine
            xile-buffer-state-pristine))

(define-record-type <xile-buffer-state>
  (make-xile-buffer-state view_id file_path bufwin pristine line_cache cursor variables)
  xile-buffer-state?
  (view_id xile-buffer-state-view_id set-xile-buffer-state-view_id) ; string : internal Xi identifier for the buffer
  (file_path xile-buffer-state-file_path set-xile-buffer-state-file_path) ; string : file path to the file attached to the buffer
  (bufwin xile-buffer-state-bufwin set-xile-buffer-state-bufwin) ; ncurses window : window displaying the buffer
  (pristine xile-buffer-state-pristine set-xile-buffer-state-pristine) ; boolean : pristine (unsaved) state
  (line_cache xile-buffer-state-line_cache set-xile-buffer-state-line_cache) ; xi-line-cache : line cache for the buffer
  (cursor xile-buffer-state-cursor set-xile-buffer-state-cursor) ; (int . int) :  Cursor position as (y . x)
  (variables xile-buffer-state-variables set-xile-buffer-state-variables)) ; alist of buffer local variables

(define (xile-buffer-setvar! state name value)
  "Set the buffer-local variable NAME to VALUE in STATE."
  (set-xile-buffer-state-variables state (assoc-set! (xile-buffer-state-variables state) (string->symbol name) value)))

(define (xile-buffer-getvar state name)
  "Get the buffer-local value of variable NAME in STATE."
  (let ((key-val-pair
         (assoc (xile-buffer-state-variables state) (string->symbol name))))
    (if key-val-pair
        (cdr key-val-pair)
        'undefined-xile-variable)))
