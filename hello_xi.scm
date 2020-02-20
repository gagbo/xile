#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ice-9 popen)
             (ice-9 threads)
             ;; (ice-9 getopt-long) ;; for CLI options
             (ice-9 rdelim)
             (json))

;; Debugging / Logging
(define (xile--debug severity message)
  (write-line (string-append severity ": "  message) (current-error-port)))

(define (xile--debug-info message)
  (xile--debug "INFO" message))

;; Xile protocol messages serialization / sending
(define (xile--msg-init)
  (scm->json-string '((method . "client_started") (params))))

(define (xile--msg-send port message)
  (xile--debug-info (string-append "Sending : " message))
  (write-line message port))

;; Xile protocol messages deserialization / receiving
(define (xile--msg-read port)
  (when (char-ready? port)
    (xile--debug-info "reading one line")
    (xile--msg-dispatch (json->scm port))))

;; Here the message is parsed JSON. To assert
(define (xile--msg-dispatch message)
  (write-line message (current-error-port))
  )

(define (xile--msg-handler port)
  (while (not (port-closed? port))
    (xile--msg-read port)))


;; Main
(define (main args)
  (let* ((xi-proc (open-input-output-pipe "xi-core"))
         (init-client (xile--msg-init))
         (listener (make-thread xile--msg-handler xi-proc)))

    ;; Flush I/O with Xi on each line
    (setvbuf xi-proc 'line)

    ;; Init code
    (xile--msg-send xi-proc init-client)

    ;; TODO : event loop thread instead of joining without timeout
    (usleep 500000)
    ;; TODO : Understand why listener thread doesn't die here
    (write-line "Because of a bug, you need to Control-C to quit for now")
    (cancel-thread listener)

    (close-pipe xi-proc)))
