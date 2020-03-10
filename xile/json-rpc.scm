;; coding: utf-8

(define-module (xile json-rpc)
  #:use-module (json)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:export (xile-rpc-send
            xile-rpc-handler
            xile-listener-thread
            xile-open
            xile-setup
            xile-register-callback))

(install-suspendable-ports!)

(define* (xile-setup #:optional (xi-path "xi-editor/rust/target/release/xi-core"))
  "Setup Xi process with pipes and listener thread.
XI-PATH is an optional path to xi-core executable, and defaults to the xi-core
built in the submodule of the repository
Return a list :
- car : listener thread
- cadr : input-port with communication from Xi process
- caddr : mutex for input-port
- cadddr : output-port with communication to Xi process
- cddddr : mutex for output-port"
  (let* ((xi-proc (xile-open xi-path))
         (port-from-xi (car xi-proc))
         (port-to-xi (cdr xi-proc))
         (listener (xile-listener-thread xile-rpc-handler port-from-xi)))
    (cons* listener port-from-xi (make-mutex) port-to-xi (make-mutex))))

;; Debugging / Logging
(define (xile-debug severity message)
  "Add MESSAGE with given SEVERITY to the current-error-port"
  (format (current-error-port) "[~a] ~a: ~a~%" (strftime "%F %T" (localtime (current-time))) severity message))

(define (xile-debug-info message)
  "Add MESSAGE with info severity to the current-error-port"
  (xile-debug "INFO" message))

;; Actual sending of the message
;; Defined in the (let ((id)) ) binding
(define xile-rpc-send #f)
;; Here the message is parsed JSON. To assert
;; Defined in the (let ((id)) ) binding
(define xile-rpc-dispatch #f)
;; Xile protocol messages serialization / sending
(define xile-register-callback #f)

;; Xile protocol messages deserialization / receiving
(define (xile-rpc-read port)
  "Consume messages from PORT, parse them, and send them to xile-rpc-dispatch function"
  (xile-debug-info "waiting for one line")
  (xile-rpc-dispatch (json->scm port)))

(define (xile-rpc-handler port)
  "Entry point of listening to Xi JSON-RPC messages from input-port PORT"
  (while (not (port-closed? port))
    (xile-rpc-read port)))

;; Socket opening function
(define (xile-open path)
  "Open a Xi process using PATH to xi-core executable.
Return a pair (input-port . output-port) for pipe communication with xi-core process."
  (let* ((xi-pipes (pipe)))
    (setvbuf (car xi-pipes) 'line)
    (setvbuf (cdr xi-pipes) 'line)
    ;; Tried with-output-to-port but it didn't work for some reason
    ;; and sent the output of xi-core process to stdout instead
    (parameterize ((current-output-port (cdr xi-pipes))
                   (current-error-port (open "logs/xi-core.log" (logior O_CREAT O_WRONLY))))
      (let ((from-xi (car xi-pipes))
            (to-xi (open-output-pipe path)))
        (cons from-xi to-xi)))))

(define (xile-listener-thread handler-proc port)
  "Return a thread calling HANDLER-PROC on a given PORT"
  (parameterize ((current-output-port (open "logs/xile-listen-out.log" (logior O_APPEND O_CREAT O_WRONLY)))
                 (current-error-port (open "logs/xile-listen-err.log" (logior O_APPEND O_CREAT O_WRONLY))))
    (make-thread handler-proc port)
    )
  )

;; HACK : Begin a let-binding to share callback registration and fetching.
(let (;; https://www.gnu.org/software/guile/manual/html_node/Callback-Closure.html#Callback-Closure
      ;; for design ideas.
      (id-to-callback (make-hash-table 31))
      (notification-to-callback (make-hash-table 31)))

  (set! xile-rpc-send (lambda (port mutex message)
                        "Send a MESSAGE for Xi process using PORT.
PORT is protected by MUTEX
MESSAGE must be a pair (id . actual-message) to help return value.
Return the id of the message sent if it was a message, or #f for a notification."
                        (let ((actual-message (cdr message))
                              (id (car message)))
                          (xile-debug-info (string-append "Sending : " actual-message))
                          (with-mutex mutex
                            (write-line actual-message port))
                          id)))

  (set! xile-register-callback (lambda (message-type handler-proc)
                                 "Register a new callback to MESSAGE-TYPE.
Message type is either :
- a number matching the id and expecting a result in the alist
  (HANDLER-PROC is called with \"result\" object)
- a symbol matching the method key in a notification from back-end to front-end
  (HANDLER-PROC is called with \"params\" object)"
                                 (cond ((number? message-type)
                                        (begin
                                          (format (current-error-port) "Adding callback for message id ~d~%" message-type)
                                          (hashq-set! id-to-callback message-type handler-proc)))
                                       ((symbol? message-type)
                                        (begin
                                          (format (current-error-port) "Adding callback for notification ~a~%" message-type)
                                          (hashq-set! notification-to-callback message-type handler-proc)))
                                       (#t (error "Not a valid message-type")))))


  (set! xile-rpc-dispatch (lambda (message)
                            "Dispatch a received MESSAGE to the correct callback."
                             ;; Example of notification :
                             (format (current-error-port) "Just received message : ~%~
                                                           ----------------------------~%~
                                                           ~y~
                                                           ----------------------------~%" message)
                             (let ((message-id (assoc-ref message "id"))
                                   (message-result (assoc-ref message "result"))
                                   (notif-method (if (assoc-ref message "method") (string->symbol (assoc-ref message "method")) #f))
                                   (notif-params (assoc-ref message "params")))
                               (cond (message-id
                                      (if (hashq-get-handle id-to-callback message-id)
                                          (apply (hashq-ref id-to-callback message-id) (list message-result))
                                          (format (current-error-port) "INFO: Missing callback for id ~d~%" message-id)))
                                     (notif-method
                                      (if (hashq-get-handle notification-to-callback notif-method)
                                          (apply (hashq-ref notification-to-callback notif-method)
                                                 (list notif-params))
                                          (format (current-error-port) "INFO: Missing callback for method ~a~%" notif-method)))
                                     (#t
                                      (format (current-error-port) "This message is unsupported : ~y~%" message)))))))
