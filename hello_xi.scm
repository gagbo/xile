#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ice-9 popen)
             (ice-9 threads)
             ;; (ice-9 getopt-long) ;; for CLI options
             (ice-9 suspendable-ports)
             (ice-9 rdelim)
             (json))

(install-suspendable-ports!)

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
  (xile--debug-info "waiting for one line")
  (xile--msg-dispatch (json->scm port)))

;; Here the message is parsed JSON. To assert
(define (xile--msg-dispatch message)
  (write-line message (current-error-port)))

(define (xile--msg-handler port)
  (while (not (port-closed? port))
    (xile--msg-read port)))

;; Socket opening function
(define (xile--open path)
  (let* ((xi-pipes (pipe)))
    (setvbuf (car xi-pipes) 'line)
    (setvbuf (cdr xi-pipes) 'line)
    ;; Tried with-output-to-port but it didn't work for some reason
    ;; and sent the output of xi-core process to stdout
    (parameterize ((current-output-port (cdr xi-pipes)))
      (let ((from-xi (car xi-pipes))
            (to-xi (open-output-pipe path)))
        (cons from-xi to-xi)))))

;; Main
(define (main args)
  (let* ((xi-proc (xile--open "xi-core"))
         (port-from-xi (car xi-proc))
         (port-to-xi (cdr xi-proc))
         (init-client (xile--msg-init))
         (listener (make-thread xile--msg-handler port-from-xi)))

    ;; Init code
    (xile--msg-send port-to-xi init-client)

    ;; TODO : event loop thread instead of joining
    (join-thread listener (+ 2 (current-time)))

    (close-port port-to-xi)
    (close-port port-from-xi)))
