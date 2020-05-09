;; coding: utf-8
;; Editor states and keymaps

(define-module (xile editor-states)
  #:use-module (ice-9 hash-table)
  #:export (normal-state
            normal-state-keymap))

;; Keymap of normal state
;; A keymap links keysequences to symbols. These symbols are called as functions later
;; (alist->hash-table '(()))
(define-once normal-state-keymap
  (alist->hash-table
   '(("q" . quit)
     ("[DOWN]" . move-down)
     ("[UP]" . move-up)
     ("[NPAGE]" . scroll-view-down)
     ("[PPAGE]" . scroll-view-up))))

;; Normal state with its keymap
(define-once normal-state '(normal . ,normal-state-keymap))
