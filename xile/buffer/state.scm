;; coding: utf-8
;; Xile buffer state and variables

(define-module (xile buffer state)
  #:use-module (xile xi-protocol notification-types)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-xile-buffer-state
            xile-buffer-setvar!
            xile-buffer-getvar
            xile-buffer-apply-config-change
            xile-buffer-state?
            xile-buffer-state-guard
            xile-buffer-state-view_id
            set-xile-buffer-state-view_id
            xile-buffer-state-file_path
            set-xile-buffer-state-file_path
            xile-buffer-state-line_cache
            set-xile-buffer-state-line_cache
            xile-buffer-state-cursor
            set-xile-buffer-state-cursor
            xile-buffer-state-bufwin
            xile-buffer-state-win-guard
            xile-buffer-state-to-xi
            xile-buffer-state-to-xi-guard
            set-xile-buffer-state-bufwin
            set-xile-buffer-state-pristine
            xile-buffer-state-pristine))

(define-record-type <xile-buffer-state>
  (make-xile-buffer-state
   state-guard view_id file_path bufwin bufwin-guard to-xi to-xi-guard pristine line_cache cursor variables)
  xile-buffer-state?
  (state-guard xile-buffer-state-guard) ; Mutex : Access to this state
  (view_id xile-buffer-state-view_id set-xile-buffer-state-view_id) ; string : internal Xi identifier for the buffer
  (file_path xile-buffer-state-file_path set-xile-buffer-state-file_path) ; string : file path to the file attached to the buffer
  (bufwin xile-buffer-state-bufwin set-xile-buffer-state-bufwin) ; ncurses window : window displaying the buffer
  (bufwin-guard xile-buffer-state-win-guard)                     ; Mutex : Access to the ncurses window
  (to-xi xile-buffer-state-to-xi)                                      ; Output Port : output-port to Xi
  (to-xi-guard xile-buffer-state-to-xi-guard)                          ; Mutex : access to the output port
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
         (assoc (string->symbol name) (xile-buffer-state-variables state))))
    (if key-val-pair
        (cdr key-val-pair)
        'undefined-xile-variable)))

(define (xile-buffer-apply-config-change state changes)
  "Apply a xi-buffer-config-change CHANGE request on STATE."
  (for-each (lambda (single-change) (xile-buffer-setvar! state (car single-change) (cdr single-change)))
            (xi-buffer-config-change-list changes)))
