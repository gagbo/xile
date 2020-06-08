;; coding: utf-8
;; Editor states and keymaps

(define-module (xile editor-states)
  #:use-module (xile std process)
  #:use-module (xile std buffer)
  #:use-module (xile variables)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 hash-table)
  #:export (insert-state
            insert-state-keymap
            switch-to-insert-state
            normal-state
            normal-state-keymap
            switch-to-normal-state
            find-binding))

(define (switch-to-insert-state)
  "Change current-state to insert-state"
  (set! current-state 'insert-state))

(define (switch-to-normal-state)
  "Change current-state to normal-state"
  (set! current-state 'normal-state))

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
   `(("q" . ,kill-xile)
     ("i" . ,switch-to-insert-state)
     ("[DOWN]" . ,move_down)
     ("[UP]" . ,move_up)
     ("[NPAGE]" . ,scroll-view-down)
     ("[PPAGE]" . ,scroll-view-up))))

;; Normal state with its keymap
(define-once normal-state `((name . "normal") (keymap . ,normal-state-keymap)))

;; Keymap of insert state
;; A keymap links keysequences to symbols. These symbols are called as functions later
;; (alist->hash-table '(()))
(define-once insert-state-keymap
  (alist->hashx-table
   keymap-hash
   keymap-assoc
   `(;; FIXME : This i binding is only here for the test
     ("q" . ,(self-insert-factory "q"))
     ("[ESC]" . ,switch-to-normal-state))))

;; Insert state with its keymap
(define-once insert-state `((name . "insert") (keymap . ,insert-state-keymap)))
