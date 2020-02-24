#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ice-9 popen)
             (ice-9 threads)
             (ice-9 rdelim)
             (ice-9 suspendable-ports)
             (json))
(install-suspendable-ports!)

(define (print-from-port port)
  (while (not (port-closed? port))
    (when (char-ready? port)
      (write-line (read-line port)))))

(define (main args)
  (let* ((io-proc (open-input-pipe "yes"))
         (listener (make-thread print-from-port io-proc)))

    (join-thread listener (+ 1 (current-time)))

    (close-pipe io-proc)))
