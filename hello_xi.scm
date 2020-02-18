#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ice-9 popen)
             (ice-9 threads)
             ;; (ice-9 getopt-long) ;; for CLI options
             (ice-9 rdelim))

(define (main args)
  (let ((xi-proc (open-input-output-pipe "xi-core")))
    ;; Sender "thread"
    (display "{\"method\":\"client_started\",\"params\":{}}\n" xi-proc)

    ;; Listener thread
    (call-with-new-thread
     (while #t
       (write (read-line xi-proc))
       (newline)))

    ;; TODO : event loop thread

    (close-pipe xi-proc)
    ))
