;; coding: utf-8

(define-module (xile callbacks)
  #:use-module (xile json-rpc)
  #:use-module (xile curses-window)
  #:use-module (xile backend-notifications)
  #:export (register-default-callbacks))

(define (register-default-callbacks)
  "Register default callbacks on Xi incoming notifications / messages"
  (begin
    (xile-register-callback
     'update
     ;; The "update" callback here is just badly extracting the text
     ;; from the first line of updates.
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "Update : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-update) (parse-xi-update result))))))

    (xile-register-callback
     'scroll_to
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "Scroll-to : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-scroll-to) (parse-xi-scroll-to result))))))

    (xile-register-callback
     'language_changed
     (lambda (result)
       (format #t "language_changed unimplemented !~%")))

    (xile-register-callback
     'config_changed
     (lambda (result)
       (format #t "config_changed unimplemented !~%")))

    (xile-register-callback
     'available_themes
     (lambda (result)
       (format #t "available_themes unimplemented !~%")))

    (xile-register-callback
     'available_plugins
     (lambda (result)
       (format #t "available_plugins unimplemented !~%")))

    (xile-register-callback
     'available_languages
     (lambda (result)
       (format #t "available_languages unimplemented !~%")))))