;; coding: utf-8
;; A Xile Buffer (a "view" from Xi point of view)

(define-module (xile buffer)
  #:use-module (xile variables)
  #:use-module (xile buffer state)
  #:use-module (xile xi-protocol message)
  #:use-module (xile xi-protocol json-rpc)
  #:use-module (xile xi-protocol notification-types)
  #:use-module (xile xi-protocol line-cache)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (make-xile-buffer
            find-xile-buffer))

(define (find-xile-buffer view_id)
      "Find a buffer from VIEW_ID. Return #f if VIEW_ID is not registered yet."
      (with-mutex id-to-buffer-guard
        (hashq-ref id-to-buffer view_id)))

(define* (make-xile-buffer port-to-xi send-mutex #:optional file_path)
    "Create a buffer with PORT-TO-XI used to send messages to xi-core process.
SEND-MUTEX protects access to PORT-TO-XI

Optional FILE_PATH can be given to set the view to point to a file.

The dispatching of the returned lambda can be checked in source code.
- cb* messages/procedures regard callbacks to back-end notifications
- Other messages/procedures usually wrap message sending to xi
- A few accessors for the variables in the closures are added

Opening multiple buffers pointing to the same FILE_PATH is undefined behaviour,
as of 2020-03-09, xi doesn't handle multiple views of a single file."
    (let* ((bufstate (make-xile-buffer-state
                      (make-mutex) #f file_path #f
                      (make-mutex) port-to-xi send-mutex #t
                      (make-xi-line-cache #() 0 0) #f '()))
           (current-view (cons 0 0))
           (to-xi (xile-buffer-state-to-xi bufstate))
           (to-xi-guard (xile-buffer-state-to-xi-guard bufstate))
           (bufwin-guard (xile-buffer-state-win-guard bufstate))
           (bufstate-guard (xile-buffer-state-guard bufstate))
           (dirty-state #f))

      (define dispatch #f)            ; dispatch acts as "this" in OOP-languages

      ;; TODO(refactor) : Refactor the notification sending out of buffer.
      ;; This is the most stateless part we can start with.
      (define (rpc-send-notif notif)
        "Send the notification NOTIF to Xi through RPC."
        (xile-rpc-send to-xi to-xi-guard notif))

      (define (scroll min-line max-line)
        "Send Scroll[MIN-LINE MAX-LINE]."
        (set! current-view (cons min-line max-line))
        (rpc-send-notif (xile-msg-edit-scroll (xile-buffer-state-view_id bufstate) min-line max-line)))

      (define (insert chars)
        "Insert CHARS"
        (set! dirty-state #t)
        (rpc-send-notif (xile-msg-edit-insert (xile-buffer-state-view_id bufstate) chars)))

      (define* (scroll-view-down #:optional (lines 1))
        "Scroll the view down LINES lines."
        (let ((current-min-line (car current-view))
              (current-max-line (cdr current-view)))
          (set! current-view (cons (+ current-min-line lines) (+ current-max-line lines)))
          (scroll (car current-view) (cdr current-view))))

      (define* (scroll-view-up #:optional (lines 1))
        "Scroll the view up LINES lines."
        (let* ((current-min-line (car current-view))
               (current-max-line (cdr current-view))
               (max-scrolled-lines (min lines current-min-line)))
          (set! current-view (cons (- current-min-line max-scrolled-lines) (- current-max-line max-scrolled-lines)))
          (scroll (car current-view) (cdr current-view))))

      (define (move_down)
        "Move the cursor down."
        (set! dirty-state #t)
        (rpc-send-notif (xile-msg-edit-move_down (xile-buffer-state-view_id bufstate))))

      (define (move_up)
        "Move the cursor up."
        (set! dirty-state #t)
        (rpc-send-notif (xile-msg-edit-move_up (xile-buffer-state-view_id bufstate))))

      (define (create-view)
        " \"\" Constructor \"\" => sends a \"new_view\" message and sets the attribute in the callback. "
        (let ((msg (xile-msg-new_view #:file_path file_path))
              (wait-for-id (make-condition-variable))
              (guard-wait (make-mutex)))
          (xile-register-callback
           (car msg)
           (lambda (result)
             (lock-mutex id-to-buffer-guard)
             (when (and (xile-buffer-state-view_id bufstate) (hashq-get-handle id-to-buffer (xile-buffer-state-view_id bufstate)))
               hashq-remove! id-to-buffer (xile-buffer-state-view_id bufstate))
             (hashq-set! id-to-buffer (string->symbol result) dispatch)
             (unlock-mutex id-to-buffer-guard)
             (set-xile-buffer-state-view_id bufstate result)
             (signal-condition-variable wait-for-id)))
          ;; HACK : using a condvar here means we need to lock/unlock a mutex.
          ;; Seems weird
          (xile-rpc-send to-xi to-xi-guard msg)
          (with-mutex id-to-buffer-guard
            (wait-condition-variable wait-for-id id-to-buffer-guard)))
        (set! dirty-state #t))

      (define (get-local-variable name)
        "Get the buffer-local variable NAME."
        (with-mutex bufstate-guard
          (xile-buffer-getvar bufstate name)))

      (define (set-local-variable! name value)
        "Set the buffer-local variable NAME to VALUE."
        (with-mutex bufstate-guard
          (xile-buffer-setvar! bufstate name value)))

      (define (cb-scroll-to scroll-to)
        "Callback to handle scroll_to message y x from Xi."
        (let ((line (xi-scroll-to-line scroll-to))
              (col (xi-scroll-to-col scroll-to)))
          (with-mutex bufwin-guard
            (set-xile-buffer-state-cursor bufstate (cons line col)))))

      (define (cb-config-changed buffer-config-changes)
        "Callback to handle config_changed message BUFFER-CONFIG-CHANGE from Xi."
        (with-mutex bufstate-guard
          (xile-buffer-apply-config-change bufstate buffer-config-changes)))

      (define (cb-language-changed buffer-language-change)
        "Callback to handle config_changed message BUFFER-LANGUAGE-CHANGE from Xi."
        (with-mutex bufstate-guard
          (xile-buffer-setvar! bufstate "language_id" (xi-buffer-language-change-id buffer-language-change))))

      (define (cb-available-plugins buffer-available-plugins)
        "Callback to handle config_changed message BUFFER-AVAILABLE-PLUGINS from Xi."
        (with-mutex bufstate-guard
          (xile-buffer-setvar! bufstate "plugins" (xi-buffer-available-plugins-list buffer-available-plugins))))

      (define (cb-plugin-started plugin-start)
        "Callback to handle plugin_started message PLUGIN-START from Xi."
        (with-mutex bufstate-guard
          (let* ((old-plugins (xile-buffer-getvar bufstate "plugins"))
                 (name (xi-buffer-plugin-started-name plugin-start))
                 (old-plugin-alist (assoc-ref old-plugins name)))
            (xile-buffer-setvar! bufstate "plugins"
                                 (assoc-set! old-plugins name (assoc-set! old-plugin-alist "running" #t))))))

      (define (plugin-state name)
        "Return the state of plugin NAME in current buffer."
        (with-mutex bufstate-guard
          (assoc-ref (xile-buffer-getvar bufstate "plugins") name)))

      (define (cb-plugin-stopped plugin-stopped)
        "Callback to handle plugin_stopped message PLUGIN-STOPPED from Xi."
        (with-mutex bufstate-guard
          (let* ((old-plugins (xile-buffer-getvar bufstate "plugins"))
                 (name (xi-buffer-plugin-stopped-name plugin-stopped))
                 (code (xi-buffer-plugin-stopped-code plugin-stopped))
                 (old-plugin-alist (assoc-ref old-plugins name))
                 (new-plugin-alist (assoc-set! (assoc-set! old-plugin-alist "running" #f) "last-code" code)))
            (xile-buffer-setvar! bufstate "plugins" new-plugin-alist))))

      (define (cb-update-cmds update-cmds)
        "Callback to handle update-cmds message UPDATE-CMDS from Xi."
        (with-mutex bufstate-guard
          (let* ((old-plugins (xile-buffer-getvar bufstate "plugins"))
                 (name (xi-buffer-update-cmds-name update-cmds))
                 (list (xi-buffer-update-cmds-list update-cmds))
                 (old-plugin-alist (assoc-ref old-plugins name))
                 (new-plugin-alist (assoc-set! old-plugin-alist "cmds" list)))
            (xile-buffer-setvar! bufstate "plugins" new-plugin-alist))))

      (define (cb-update result)
        "Callback to handle update message RESULT from Xi."
        (with-mutex bufstate-guard
          (set-xile-buffer-state-pristine bufstate (xi-update-pristine result))
          (set-xile-buffer-state-line_cache
           bufstate
           (xi-line-cache-execute-update (xile-buffer-state-line_cache bufstate) result))
          (set! dirty-state #t)))

      (define (need-redisplay)
        "Return #t if the buffer should be redisplayed."
        dirty-state)

      (define (set-clean-state)
        "Set the buffer clean (= in sync with its display)"
        (set! dirty-state #f))

      (define (current-visible-lines)
        "Return (min-line . max-line) visible."
        current-view)

      (set! dispatch
        (lambda (m)
          (cond ((eq? m 'get-bufstate) bufstate)
                ((eq? m 'get-name) (xile-buffer-state-file_path bufstate))
                ((eq? m 'get-var) get-local-variable)
                ((eq? m 'set-var!) set-local-variable!)
                ((eq? m 'create-view) create-view)
                ((eq? m 'insert) insert)
                ((eq? m 'scroll) scroll)
                ((eq? m 'scroll-view-up) scroll-view-up)
                ((eq? m 'scroll-view-down) scroll-view-down)
                ((eq? m 'move_up) move_up)
                ((eq? m 'move_down) move_down)
                ((eq? m 'cb-scroll-to) cb-scroll-to)
                ((eq? m 'cb-update) cb-update)
                ((eq? m 'cb-config-changed) cb-config-changed)
                ((eq? m 'cb-language-changed) cb-language-changed)
                ((eq? m 'cb-available-plugins) cb-available-plugins)
                ((eq? m 'cb-plugin-started) cb-plugin-started)
                ((eq? m 'cb-plugin-stopped) cb-plugin-stopped)
                ((eq? m 'cb-update-cmds) cb-update-cmds)
                ((eq? m 'plugin-state) plugin-state)
                ((eq? m 'need-redisplay) need-redisplay)
                ((eq? m 'set-clean-display-state) set-clean-state)
                ((eq? m 'current-view) current-visible-lines)
                (else (error (format #f "Unknown request : XILE BUFFER ~a~%" m))))))

      dispatch))
