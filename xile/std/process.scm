;; coding: utf-8
;; Standard library of main process functions

(define-module (xile std process)
  #:use-module (ice-9 optargs)
  #:use-module (xile variables)
  #:export (kill-xile))

(define* (kill-xile #:key (reason "Called the quit function"))
  "Print a REASON for quitting. The function is a marker to stop the event loop"
  (format #t "Quitting Xile : ~s~%" reason))
