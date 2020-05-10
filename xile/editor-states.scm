;; coding: utf-8
;; Editor states and keymaps

(define-module (xile editor-states)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 hash-table)
  #:export (normal-state
            normal-state-keymap
            find-binding))

;; Define functions to make keybindings alist work on strings as keys
(define (keymap-hash str size)
  (remainder (string-hash str) size))
(define (keymap-assoc str alist)
  (find (lambda (pair) (string=? str (car pair))) alist))
(define (find-binding keymap key-sequence)
  "Find a binding associated to KEY-SEQUENCE in KEYMAP.

Return #f if nothing found."
  (hashx-ref keymap-hash keymap-assoc keymap key-sequence))

;; Keymap of normal state
;; A keymap links keysequences to symbols. These symbols are called as functions later
;; (alist->hash-table '(()))
(define-once normal-state-keymap
  (alist->hashx-table
   keymap-hash
   keymap-assoc
   '(("q" . quit)
     ("[DOWN]" . move_down)
     ("[UP]" . move_up)
     ("[NPAGE]" . scroll-view-down)
     ("[PPAGE]" . scroll-view-up))))

;; Normal state with its keymap
(define-once normal-state `((name . "normal") (keymap . ,normal-state-keymap)))
