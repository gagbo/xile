#!/usr/bin/env -S guile --no-auto-compile -s
!#

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (srfi srfi-64)
             (xile sdl input)
             (sdl2 events))

;; Encode keys
(test-begin "encode-keys-sdl")

(test-equal "Encode simple letter"
  "q"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'q 'q '())))

(test-equal "Encode left shift letter"
  "L"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'l 'l '(left-shift))))

(test-equal "Encode right shift letter"
  "L"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'l 'l '(right-shift))))

(test-equal "Encode special key"
  "[DOWN]"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'down 'down '())))

(test-equal "Encode C0 control characters"
  "[RET]"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'return 'return '())))

(test-equal "Encode C-m properly"
  "[C-m]"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'm 'm '(left-control))))

(test-equal "Encode escape char"
  "[["
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'left-bracket 'left-bracket '())))

(test-equal "Encode control letter"
  "[C-d]"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'd 'd '(left-control))))

(test-equal "Encode alt letter"
  "[M-x]"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'x 'x '(right-alt))))

(test-equal "Encode control alt shift letter"
  "[C-M-P]"
  (keyboard-event->string-sequence
   (make-keyboard-event 12 4 #t #f 'p 'p '(left-alt right-control right-shift))))

(test-end "encode-keys-sdl")


;; Key Sequence Length accessor
(test-begin "key-sequence-length-sdl")

(test-equal "Empty key sequence"
  0
  (key-sequence-length ""))

(test-equal "One key sequence - normal"
  1
  (key-sequence-length "o"))

(test-equal "One key sequence - special"
  1
  (key-sequence-length "[M-x]"))

(test-equal "Multiple key sequence - normal"
  2
  (key-sequence-length "L q"))

(test-equal "Multiple key sequence - special"
  2
  (key-sequence-length "[NPAGE] [C-k]"))

(test-end "key-sequence-length-sdl")


;; Key sequence Add API
(test-begin "key-sequence-add-sdl")

(test-equal "Empty key sequence"
  "[["
  (key-sequence-add "" "[["))

(test-equal "Some key sequence"
  "x L [M-S-c]"
  (key-sequence-add "x L" "[M-S-c]"))

(test-end "key-sequence-add-sdl")

(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
