;; coding: utf-8

(define-module (xile sdl callbacks)
  #:use-module (sdl2)
  #:use-module (sdl2 ttf)
  #:use-module (sdl2 render)
  #:use-module (sdl2 surface)
  #:use-module (sdl2 video)
  #:use-module (xile buffer)
  #:use-module (xile xi-protocol json-rpc)
  #:use-module (xile xi-protocol notification-types)
  #:use-module (xile xi-protocol themes)            ; Only for the 'theme_changed callback
  #:use-module (xile variables)
  #:use-module (xile sdl window)
  #:export (register-default-sdl-callbacks))

(define (redraw buffer window)
  "Redraw the buffer BUFFER in WINDOW."
  (call-with-renderer
   (make-renderer window)
   (lambda (renderer)
     (draw-buffer-in-sdl current-buffer
                         (car (window-size window))
                         (cadr (window-size window))
                         renderer))))

(define (register-default-sdl-callbacks main-win)
  "Register default sdl callbacks on Xi incoming notifications / messages using :

- MAIN-WIN to display buffer"

  (begin
    (xile-register-callback
     'update
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "Update : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-update) (parse-xi-update result))
           (when (eq? xile-buffer current-buffer)
             (redraw current-buffer main-win))))))

    (xile-register-callback
     'scroll_to
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "Scroll-to : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-scroll-to) (parse-xi-scroll-to result))
           (when (eq? xile-buffer current-buffer)
             (redraw current-buffer main-win))))))

    (xile-register-callback
     'language_changed
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "language_changed : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-language-changed) (parse-xi-buffer-language-change result))
           (when (eq? xile-buffer current-buffer)
             (redraw current-buffer main-win))))))

    (xile-register-callback
     'config_changed
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "config_changed : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-config-changed) (parse-xi-buffer-config-change result))
           (when (eq? xile-buffer current-buffer)
             (redraw current-buffer main-win))))))

    (xile-register-callback
     'theme_changed
     (lambda (result)
       (let* ((theme-changes (parse-xi-theme-changed result))
              (new-theme (make-xile-theme (xi-theme-changed-name theme-changes)
                                          (xi-theme-changed-settings theme-changes))))
         (set! current-theme new-theme)
         (format #t "current-theme set to ~a ~%" (xile-theme-name current-theme)))))

    (xile-register-callback
     'available_themes
     (lambda (result)
       (set! themes-available (xi-available-themes-list (parse-xi-available-themes result)))
       (format #t "themes-available set to ~a ~%" themes-available)))

    (xile-register-callback
     'available_plugins
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "available_plugins : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-available-plugins) (parse-xi-buffer-available-plugins result))))))

    (xile-register-callback
     'plugin_started
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "plugin_stopped : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-plugin-started) (parse-xi-plugin-started result))))))

    (xile-register-callback
     'plugin_stopped
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "plugin_stopped : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-plugin-stopped) (parse-xi-plugin-stopped result))))))

    (xile-register-callback
     'update_cmds
     (lambda (result)
       (let* ((view_id (assoc-ref result "view_id"))
              (xile-buffer (find-xile-buffer (string->symbol view_id))))
         (format #t "update_cmds : xile-buffer for ~a is ~a~%" view_id xile-buffer)
         (when xile-buffer
           ((xile-buffer 'cb-update-cmds) (parse-xi-update-cmds result))))))

    (xile-register-callback
     'add_status_item
     (lambda (result)
       (let ((key (assoc-ref result "key"))
             (align (assoc-ref result "alignment"))
             (source (assoc-ref result "source"))
             (value (assoc-ref result "value")))
         (set! status-bar (assoc-set! status-bar key `((alignment . ,(string->symbol align)) (source . ,source) (value . ,value))))
         ;; TODO(curses) : define a (draw-footer) in xile sdl-window and use that
         )))

    (xile-register-callback
     'update_status_item
     (lambda (result)
       (let* ((key (assoc-ref result "key"))
              (value (assoc-ref result "value"))
              (old-status-item (assoc-ref status-bar key))
              (new-status-item (assoc-set! old-status-item 'value value)))
         (set! status-bar (assoc-set! status-bar key new-status-item))
         ;; TODO(curses) : redraw footer using (missing) xile sdl-window (draw-footer)
         )))

    (xile-register-callback
     'remove_status_item
     (lambda (result)
       (let ((key (assoc-ref result "key")))
         (set! status-bar (assoc-remove! status-bar key))
         ;; TODO(curses) : redraw footer using (missing) xile sdl-window (draw-footer)
         )))

    (xile-register-callback
     'available_languages
     (lambda (result)
       (set! languages-available (xi-available-languages-list (parse-xi-available-languages result)))
       (format #t "languages-available set to ~a ~%" languages-available)))))
