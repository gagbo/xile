#!/usr/bin/env -S guile --no-auto-compile -s
!#

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (srfi srfi-64)
             (xile curses-window)
             (ncurses curses))

(test-begin "encode-keys")

(test-equal "Encode simple letter"
  "q"
  (encode-key-to-string-sequence #\q))

(test-equal "Encode shift letter"
  "L"
  (encode-key-to-string-sequence #\L))

(test-equal "Encode special key"
  "[DOWN]"
  (encode-key-to-string-sequence KEY_DOWN))

(test-equal "Encode C0 control characters"
  "[RET]"
  (encode-key-to-string-sequence #\cr))

(test-equal "Encode escape char"
  "[["
  (encode-key-to-string-sequence #\[))

;; TODO : Find out what's sent on C-key
;; (test-equal "Encode control letter"
;;   "[C-d]"
;;   (encode-key-to-string-sequence "^D"))

;; CANCELLED : Find out what's sent on M-key
;; M-key looks like unsupported on curses :(
;; (test-equal "Encode control letter"
;;   "[M-x]"
;;   (encode-key-to-string-sequence "^[X"))

(test-end "encode-keys")

(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
