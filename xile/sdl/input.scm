;; coding: utf-8


(define-module (xile sdl input)
  #:use-module (sdl2)
  #:use-module (sdl2 events)
  #:use-module (xile variables)             ; For global variables
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)                ; Find
  #:export (keyboard-event->string-sequence
            key-sequence-length
            key-sequence-add))

(define (keyboard-event->string-sequence event)
  "Convert a keyboard event EVENT coming from SDL event to a string sequence.

Control-key is translated to [C-key] (brackets necessary for the keymaps as well).
Alt-key is translated to [M-key].
Actual [ is encoded as [[.
Special keys (like KEY_DOWN) or (KEY_PPAGE) are encoded as [DOWN] or [PPAGE].
Space is encoded as [SPC].

Since the keymap will just look for the concatenation of the keys in its hash-table, there is no
need to make a special treatment otherwise. The bracket format is only use to disambiguate
Control-M from hitting C then - then M."
  (let ((modifiers (keyboard-event-modifiers event)))
    (let ((modifiers-sans-shift
           (filter
            (lambda (modifier)
              (cond
               ((eq? modifier 'left-shift) #f)
               ((eq? modifier 'right-shift) #f)
               (else #t)))
            modifiers))
          (modifiers-alt?
           (find
            (lambda (modifier)
              (or (eq? modifier 'left-alt) (eq? modifier 'right-alt)))
            modifiers))
          (modifiers-gui?
           (find
            (lambda (modifier)
              (or (eq? modifier 'left-gui) (eq? modifier 'right-gui)))
            modifiers))
          (modifiers-shift?
           (find
            (lambda (modifier)
              (or (eq? modifier 'left-shift) (eq? modifier 'right-shift)))
            modifiers))
          (modifiers-control?
           (find
            (lambda (modifier)
              (or (eq? modifier 'left-control) (eq? modifier 'right-control)))
            modifiers))
          (lowercase-key-name (symbol->string (keyboard-event-key event)))
          (uppercase-key-name (string-upcase (symbol->string (keyboard-event-key event)))))

      (string-concatenate
       (list
;;; Modifiers (and necessary bracket if relevant)
        (cond ((zero? (length modifiers-sans-shift))
               "")
              (else
               (fold (lambda (pair prev)
                       (match pair
                         ((mod-p . str-repr)
                          (if mod-p
                              (string-concatenate (list prev str-repr))
                              prev))))
                     "["
                     `((,modifiers-control? . "C-")
                       (,modifiers-alt?     . "M-")
                       (,modifiers-gui?     . "S-")))))

;;; Letter / keycode handling
        (cond
;;;; Escaped char
         ((eq? (keyboard-event-key event) 'left-bracket)
          "[[")
;;;; Return
         ((eq? (keyboard-event-key event) 'return)
          "[RET]")
         ((eq? (keyboard-event-key event) 'page-up)
          "[PPAGE]")
         ((eq? (keyboard-event-key event) 'page-down)
          "[NPAGE]")
;;;; Arrows and other special keys that just get UPCASED
         ((member (keyboard-event-key event) '(down up left right) eq?)
          (string-concatenate (list "[" uppercase-key-name "]")))
;;;; Unknown land (hopefully only alphas)
         (else
          (if modifiers-shift?
              uppercase-key-name
              lowercase-key-name)))

;;; Closing bracket if relevant
        (if (zero? (length modifiers-sans-shift))
            ""
            "]"))))))

(define (key-sequence-length sequence)
  "Return the number of inputs in SEQUENCE."
  (cond
   ((string-null? sequence) 0)
   (else
    (fold (lambda (char acc)
            (if (char=? char #\space)
                (+ 1 acc)
                acc))
          1
          (string->list sequence)))))

(define (key-sequence-add sequence new-key)
  "Return a sequence consisting of SEQUENCE with NEW-KEY appended."
  (cond
   ((string-null? sequence) new-key)
   (else
    (string-concatenate (list sequence " " new-key)))))
