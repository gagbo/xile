#!/usr/bin/env -S guile --no-auto-compile -s
!#

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (srfi srfi-64)
             (xile sdl window)
             (sdl2 events))

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

(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
