#!/usr/bin/env -S guile -e main -s
!#
;; coding: utf-8
(use-modules (ice-9 popen)
             (ice-9 threads)
             ;; (ice-9 getopt-long) ;; for CLI options
             (ice-9 suspendable-ports)
             (ice-9 rdelim)
             (json)
             (ncurses curses))


(install-suspendable-ports!)

;; Debugging / Logging
(define (xile--debug severity message)
  (write-line (string-append severity ": "  message) (current-error-port)))

(define (xile--debug-info message)
  (xile--debug "INFO" message))

;; Actual sending of the message
;; Defined in the (let ((id)) ) binding
(define xile--msg-send #f)
;; Here the message is parsed JSON. To assert
;; Defined in the (let ((id)) ) binding
(define xile--msg-dispatch #f)

;; Xile protocol messages deserialization / receiving
(define (xile--msg-read port)
  (xile--debug-info "waiting for one line")
  (xile--msg-dispatch (json->scm port)))

(define (xile--msg-handler port)
  (while (not (port-closed? port))
    (xile--msg-read port)))

;; Socket opening function
(define (xile--open path)
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
  (parameterize ((current-output-port (open "logs/xile-listen-out.log" (logior O_APPEND O_CREAT O_WRONLY)))
                 (current-error-port (open "logs/xile-listen-err.log" (logior O_APPEND O_CREAT O_WRONLY))))
    (make-thread handler-proc port)
    )
  )

(define (xile-setup)
  (let* ((xi-proc (xile--open "xi-editor/rust/target/release/xi-core"))
         (port-from-xi (car xi-proc))
         (port-to-xi (cdr xi-proc))
         (init-client (xile--msg-init '()))
         (listener (xile-listener-thread xile--msg-handler port-from-xi)))
    (cons listener (cons port-from-xi port-to-xi))))

;; Xile protocol messages serialization / sending
;; TODO : use optional params to fill the message as necessary
(define (xile--notif-generic method param-list)
  (cons #f (scm->json-string `((method . ,method) (params . ,param-list)))))

(define xile--msg-generic #f)
;; TODO : Add a (id -> procedure) map to the let-binding to register/fetch callbacks
(let ((id 0))
  (set! xile--msg-send (lambda (port message)
                         (let ((actual-message (cdr message))
                               (id (car message)))
                           (xile--debug-info (string-append "Sending : " actual-message))
                           ;; TODO Add the callback
                           (write-line actual-message port)
                           id)))

  (set! xile--msg-dispatch (lambda (message)
                             (write-line message (current-error-port))))

  (set! xile--msg-generic (lambda (method param-list)
                           (set! id (1+ id))
                           (cons id (scm->json-string `((id . ,id) (method . ,method) (params . ,param-list)))))))

;; TODO : namespace should be a string or a symbol
(define (xile--msg-namespace-generic namespace method param-list)
  (xile--msg-generic namespace (acons 'method method param-list)))

;; client_started {"config_dir" "some/path"?, "client_extras_dir": "some/other/path"?}
;;
;; Sent by the client immediately after establishing the core connection.
;; This is used to perform initial setup. The two arguments are optional;
;; the config_dir points to a directory where the user’s config files and plugins live,
;; and the client_extras_dir points to a directory where the frontend can package
;; additional resources, such as bundled plugins.
(define (xile--msg-init param-list)
  (xile--notif-generic 'client_started param-list))

;; new_view { "file_path": "path.md"? } -> "view-id-1"
;;
;; Creates a new view, returning the view identifier as a string. file_path is optional;
;; if specified, the file is loaded into a new buffer; if not a new empty buffer is created.
;; Currently, only a single view into a given file can be open at a time.
;; Note:, there is currently no mechanism for reporting errors. Also note,
;; the protocol delegates power to load and save arbitrary files.
;; Thus, exposing the protocol to any other agent than a front-end in direct control
;; should be done with extreme caution.
(define (xile--msg-new_view param-list)
  (xile--msg-generic 'new_view param-list))

;; close_view {"view_id": "view-id-1"}
;;
;; Closes the view associated with this view_id.
(define (xile--msg-close_view param-list)
  (xile--msg-generic 'close_view param-list))

;; save {"view_id": "view-id-4", "file_path": "save.txt"}
;;
;; Saves the buffer associated with view_id to file_path.
;; See the note for new_view. Errors are not currently reported.
(define (xile--msg-save param-list)
  (xile--msg-generic 'save param-list))

;; set_theme {"theme_name": "InspiredGitHub"}
;;
;; Asks core to change the theme. If the change succeeds the client will receive
;; a theme_changed notification.
(define (xile--msg-set_theme param-list)
  (xile--msg-generic 'set_theme param-list))

;; set_language {"view-id":"view-id-1", "language_id":"Rust"}
;;
;; Asks core to change the language of the buffer associated with the view_id.
;; If the change succeeds the client will receive a language_changed notification.
(define (xile--msg-set_language param-list)
  (xile--msg-generic 'set_language param-list))

;; modify_user_config { "domain": Domain, "changes": Object }
;;
;; Modifies the user’s config settings for the given domain.
;; Domain should be either the string "general" or an object of the form
;; {"syntax": "rust"}, or {"user_override": "view-id-1"},
;; where "rust" is any valid syntax identifier, and "view-id-1" is
;; the identifier of any open view.
(define (xile--msg-modify_user_config param-list)
  (xile--msg-generic 'modify_user_config param-list))

;; get_config {"view_id": "view-id-1"} -> Object
;;
;; Returns the config table for the view associated with this view_id.
(define (xile--msg-get_config param-list)
  (xile--msg-generic 'get_config param-list))

;; edit {"method": "insert", "params": {"chars": "A"}, "view_id": "view-id-4"}
;;
;; Dispatches the inner method to the per-tab handler,
;; with individual inner methods described below:
(define (xile--msg-edit-generic method view_id param-list)
  (xile--msg-namespace-generic 'edit method `((view_id . ,view_id) (params . ,param-list))))

;; insert {"chars":"A"}
;;
;; Inserts the chars string at the current cursor locations.
(define (xile--msg-edit-insert view_id param-list)
  (xile--msg-edit-generic 'insert view_id param-list))

;; paste {"chars": "password"}
;;
;; Inserts the chars string at the current cursor locations. If there are
;; multiple cursors and chars has the same number of lines as there are cursors,
;; one line will be inserted at each cursor, in order; otherwise the full string
;; will be inserted at each cursor.
(define (xile--msg-edit-paste view_id param-list)
  (xile--msg-edit-generic 'paste view_id param-list))

;; copy -> String|Null
;;
;; Copies the active selection, returning their contents or Null if the
;; selection was empty.
(define (xile--msg-edit-copy view_id)
  (xile--msg-edit-generic 'copy view_id '()))

;; cut -> String|Null
;;
;; Cut the active selection, returning their contents or Null if the selection
;; was empty
(define (xile--msg-edit-cut view_id)
  (xile--msg-edit-generic 'cut view_id '()))

;; scroll [0,18]
;;
;; Notifies the back-end of the visible scroll region, defined as the first and
;; last (non-inclusive) formatted lines. The visible scroll region is used to
;; compute movement distance for page up and page down commands, and also
;; controls the size of the fragment sent in the update method.
;;
;; TODO : notification, not method
(define (xile--msg-edit-scroll view_id start end)
  (xile--msg-edit-generic 'scroll view_id #(start end)))

;; resize {width: 420, height: 400}
;;
;; Notifies the backend that the size of the view has changed. This is used for
;; word wrapping, if enabled. Width and height are specified in px units /
;; points, not display pixels.
(define (xile--msg-edit-resize view_id width height)
  (xile--msg-edit-generic 'resize view_id `((width . ,width) (height . ,height))))

;; click [42,31,0,1]
;;
;; Implements a mouse click. The array arguments are: line and column (0-based,
;; utf-8 code units), modifiers (again, 2 is shift), and click count.
(define (xile--msg-edit-click view_id line column modifiers count)
  (xile--msg-edit-generic 'click view_id #(line column modifiers count)))

;; drag [42,32,0]
;;
;; Implements dragging (extending a selection). Arguments are line, column, and
;; flag as in click.
(define (xile--msg-edit-drag view_id line column flag)
  (xile--msg-edit-generic 'drag view_id #(line column flag)))

;; gesture {"line": 42, "col": 31, "ty": "toggle_sel"}
;;
;; Note: both click and drag functionality will be migrated to additional ty options for gesture.
;;
;; Currently, the following gestures are supported:
;;
;; point_select # moves the cursor to a point
;; toggle_sel # adds or removes a selection at a point
;; range_select # modifies the selection to include a point (shift+click)
;; line_select # sets the selection to a given line
;; word_select # sets the selection to a given word
;; multi_line_select # adds a line to the selection
;; multi_word_select # adds a word to the selection
(define (xile--msg-edit-gesture view_id gesture line column)
  (xile--msg-edit-generic 'gesture view_id `((ty . ,gesture) (line . ,line) (col . ,column))))

;; goto_line {"line": 1}
;;
;; Sets the cursor to the beginning of the provided line and scrolls to this
;; position.
(define (xile--msg-edit-goto_line view_id line)
  (xile--msg-edit-generic 'goto_line view_id `( (line . ,line) )))

;; Other movement and deletion commands
;;
;; The following edit methods take no parameters, and have similar meanings as
;; NSView actions. The pure movement and selection modification methods will be
;; migrated to a more general method that takes a “movement” enum as a
;; parameter.

;; delete_backward
(define (xile--msg-edit-delete_backward view_id)
  (xile--msg-edit-generic 'delete_backward view_id '()))

;; delete_forward
(define (xile--msg-edit-delete_forward view_id)
  (xile--msg-edit-generic 'delete_forward view_id '()))

;; insert_newline
(define (xile--msg-edit-insert_newline view_id)
  (xile--msg-edit-generic 'insert_newline view_id '()))

;; duplicate_line
(define (xile--msg-edit-duplicate_line view_id)
  (xile--msg-edit-generic 'duplicate_line view_id '()))

;; move_up
(define (xile--msg-edit-move_up view_id)
  (xile--msg-edit-generic 'move_up view_id '()))

;; move_up_and_modify_selection
(define (xile--msg-edit-move_up_and_modify_selection view_id)
  (xile--msg-edit-generic 'move_up_and_modify_selection view_id '()))

;; move_down
(define (xile--msg-edit-move_down view_id)
  (xile--msg-edit-generic 'move_down view_id '()))

;; move_down_and_modify_selection
(define (xile--msg-edit-move_down_and_modify_selection view_id)
  (xile--msg-edit-generic 'move_down_and_modify_selection view_id '()))

;; move_left
(define (xile--msg-edit-move_left view_id)
  (xile--msg-edit-generic 'move_left view_id '()))

;; move_left_and_modify_selection
(define (xile--msg-edit-move_left_and_modify_selection view_id)
  (xile--msg-edit-generic 'move_left_and_modify_selection view_id '()))

;; move_right
(define (xile--msg-edit-move_right view_id)
  (xile--msg-edit-generic 'move_right view_id '()))

;; move_right_and_modify_selection
(define (xile--msg-edit-move_right_and_modify_selection view_id)
  (xile--msg-edit-generic 'move_right_and_modify_selection view_id '()))

;; scroll_page_up
(define (xile--msg-edit-scroll_page_up view_id)
  (xile--msg-edit-generic 'scroll_page_up view_id '()))

;; page_up_and_modify_selection
(define (xile--msg-edit-page_up_and_modify_selection view_id)
  (xile--msg-edit-generic 'page_up_and_modify_selection view_id '()))

;; scroll_page_down
(define (xile--msg-edit-scroll_page_down view_id)
  (xile--msg-edit-generic 'scroll_page_down view_id '()))

;; page_down_and_modify_selection
(define (xile--msg-edit-page_down_and_modify_selection view_id)
  (xile--msg-edit-generic 'page_down_and_modify_selection view_id '()))

;; yank
(define (xile--msg-edit-yank view_id)
  (xile--msg-edit-generic 'yank view_id '()))

;; transpose
(define (xile--msg-edit-transpose view_id)
  (xile--msg-edit-generic 'transpose view_id '()))

;; select_all
(define (xile--msg-edit-select_all view_id)
  (xile--msg-edit-generic 'select_all view_id '()))

;; collapse_selections
(define (xile--msg-edit-collapse_selections view_id)
  (xile--msg-edit-generic 'collapse_selections view_id '()))

;; add_selection_above
(define (xile--msg-edit-add_selection_above view_id)
  (xile--msg-edit-generic 'add_selection_above view_id '()))

;; add_selection_below
(define (xile--msg-edit-add_selection_below view_id)
  (xile--msg-edit-generic 'add_selection_below view_id '()))

;; Transformations
;;
;; The following methods act by modifying the current selection.

;; uppercase
(define (xile--msg-edit-uppercase view_id)
  (xile--msg-edit-generic 'uppercase view_id '()))

;; lowercase
(define (xile--msg-edit-lowercase view_id)
  (xile--msg-edit-generic 'lowercase view_id '()))

;; capitalize
(define (xile--msg-edit-capitalize view_id)
  (xile--msg-edit-generic 'capitalize view_id '()))

;; indent
(define (xile--msg-edit-indent view_id)
  (xile--msg-edit-generic 'indent view_id '()))

;; outdent
(define (xile--msg-edit-outdent view_id)
  (xile--msg-edit-generic 'outdent view_id '()))

;; Number Transformations
;;
;; The following methods work with a caret or multiple selections. If the
;; beginning of a selection (or the caret) is within a positive or negative
;; number, the number will be transformed accordingly:

;; increase_number
(define (xile--msg-edit-increase_number view_id)
  (xile--msg-edit-generic 'increase_number view_id '()))

;; decrease_number
(define (xile--msg-edit-decrease_number view_id)
  (xile--msg-edit-generic 'decrease_number view_id '()))

;; Recording
;;
;; These methods allow manipulation and playback of event recordings.
;;
;; If there is no currently active recording, start recording events under the
;; provided name.
;;
;; If there is no provided name, the current recording is saved.
;;
;; If the name provided matches the current recording name, the current
;; recording is saved.
;;
;; If the name provided does not match the current recording name, the events
;; for the current recording are dismissed.
(define (xile--msg-edit-toggle_recording view_id record_name)
  (xile--msg-edit-generic 'toggle_recording view_id `((recording_name . ,record_name))))

(define (xile--msg-edit-play_recording view_id record_name)
  (xile--msg-edit-generic 'play_recording view_id `((recording_name . ,record_name))))

(define (xile--msg-edit-clear_recording view_id record_name)
  (xile--msg-edit-generic 'clear_recording view_id `((recording_name . ,record_name))))

;; LSP stuff in edit namespace

;; Hover
;;
;; Get Hover for a position in file. The request for hover is made as a
;; notification. The client is forwarded result back via a show_hover rpc
;;
;; If position is skipped in the request, current cursor position will be used
;; in core.
(define (xile--msg-edit-request_hover view_id req_id line column)
  (xile--msg-edit-generic 'request_hover view_id `((request_id . ,req_id) (position . ((line . ,line) (column . ,column))))))

;; Plugin namespace
;;
;; Note: plugin commands are in flux, and may change.
;;
;; Example: The following RPC dispatches the inner method to the plugin manager.
;;
;; plugin {"method": "start", params: {"view_id": "view-id-1", plugin_name:
;; "syntect"}}
(define (xile--msg-plugin-generic method view_id param-list)
  (xile--msg-namespace-generic 'plugin method `((params . ,(acons 'view_id view_id param-list)))))

;; Starts the named plugin for the given view.
(define (xile--msg-plugin-start view_id plugin_name)
  (xile--msg-plugin-generic 'start view_id `((plugin_name . ,plugin_name))))

;; Stops the named plugin for the given view.
(define (xile--msg-plugin-stop view_id plugin_name)
  (xile--msg-plugin-generic 'stop view_id `((plugin_name . ,plugin_name))))

;; Sends a custom rpc command to the named receiver. This may be a notification
;; or a request.
;;
;; plugin_rpc {"view_id": "view-id-1", "receiver": "syntect",
;; "notification": {
;;     "method": "custom_method",
;;     "params": {"foo": "bar"},
;; }}
(define (xile--msg-plugin-rpc view_id plugin_name request)
  (xile--msg-plugin-generic 'stop view_id `((receiver . ,plugin_name) ,request)))

;; find {"chars": "a", "case_sensitive": false, "regex": false, "whole_words":
;; true} Parameters regex and whole_words are optional and by default false.
;;
;; Sets the current search query and options.
(define (xile--msg-plugin-find view_id chars case_sen regex whole_words)
  (xile--msg-plugin-generic 'find view_id `((chars . ,chars) (case_sensitive . ,case_sen) (regex . ,regex) (whole_words . ,whole_words))))

;; This find command supports multiple search queries.
;;
;; multi_find [{"id": 1, "chars": "a", "case_sensitive": false, "regex": false,
;; "whole_words": true}] Parameters regex and whole_words are optional and by
;; default false. id is an optional parameter used to uniquely identify a search
;; query. If left empty, the query is considered as a new query and the backend
;; will generate a new ID.
;;
;; Sets the current search queries and options.
(define (xile--msg-plugin-multi_find view_id id chars case_sen regex whole_words)
  (xile--msg-plugin-generic 'multi_find view_id `((id . ,id) (chars . ,chars) (case_sensitive . ,case_sen) (regex . ,regex) (whole_words . ,whole_words))))

;; find_next {"wrap_around": true, "allow_same": false, "modify_selection": "set"}
;; find_previous {"wrap_around": true, "allow_same": false, "modify_selection": "set"}
;;
;; All parameters are optional. Boolean parameters are by default false and
;; modify_selection is set by default. If allow_same is set to true the current
;; selection is considered a valid next occurrence. Supported options for
;; modify_selection are:
;;
;; none: the selection is not modified
;; set: the next/previous match will be set as the new selection
;; add: the next/previous match will be added to the current selection
;; add_removing_current: the previously added selection will be removed and the
;;   next/previous match will be added to the current selection
;;
;; Selects the next/previous occurrence matching the search query.
(define (xile--msg-plugin-find_next view_id wrap_around allow_same modify_selection)
  (xile--msg-plugin-generic 'find_next view_id `((wrap_around . ,wrap_around) (allow_same . ,allow_same) (modify_selection . ,modify_selection))))
(define (xile--msg-plugin-find_previous view_id wrap_around allow_same modify_selection)
  (xile--msg-plugin-generic 'find_previous view_id `((wrap_around . ,wrap_around) (allow_same . ,allow_same) (modify_selection . ,modify_selection))))

;; find_all { }
;;
;; Selects all occurrences matching the search query.
(define (xile--msg-plugin-find_all view_id)
  (xile--msg-plugin-generic 'find_all view_id '()))

;; highlight_find {"visible": true}
;;
;; Shows/hides active search highlights.
(define (xile--msg-plugin-highlight_find view_id visible)
  (xile--msg-plugin-generic 'highlight_find view_id `((visible . ,visible))))

;; selection_for_find {"case_sensitive": false}
;; The parameter case_sensitive is optional and false if not set.
;;
;; Sets the current selection as the search query.
(define (xile--msg-plugin-selection_for_find view_id case_sen)
  (xile--msg-plugin-generic 'selection_for_find view_id `((case_sensitive . ,case_sen))))

;; replace {"chars": "a", "preserve_case": false}
;; The parameter preserve_case is currently not implemented and ignored.
;;
;; Sets the replacement string.
(define (xile--msg-plugin-replace view_id chars preserve_case)
  (xile--msg-plugin-generic 'replace view_id `((chars . ,chars) (preserve_case . ,preserve_case))))

;; selection_for_replace {"case_sensitive": false}
;; The parameter case_sensitive is optional and false if not set.
;;
;; Sets the current selection as the replacement string.
(define (xile--msg-plugin-selection_for_replace view_id case_sen)
  (xile--msg-plugin-generic 'selection_for_replace view_id `((case_sensitive . ,case_sen))))

;; replace_next { }
;;
;; Replaces the next matching occurrence with the replacement string.
(define (xile--msg-plugin-replace_next view_id)
  (xile--msg-plugin-generic 'replace_next view_id '()))

;; replace_all { }
;;
;; Replaces all matching occurrences with the replacement string.
(define (xile--msg-plugin-replace_all view_id)
  (xile--msg-plugin-generic 'replace_all view_id '()))

;; selection_into_lines { }
;;
;; Splits all current selections into lines.
(define (xile--msg-plugin-selection_into_lines view_id)
  (xile--msg-plugin-generic 'selection_into_lines view_id '()))

;; Main
(define (main args)
  (let* ((xi-setup (xile-setup))
         (port-from-xi (cadr xi-setup))
         (port-to-xi (cddr xi-setup))
         (listener (car xi-setup)))

    (set-current-error-port (open "logs/xile-err.log" (logior O_APPEND O_CREAT O_WRONLY)))
    (set-current-output-port (open "logs/xile-out.log" (logior O_APPEND O_CREAT O_WRONLY)))

    ;; (define stdscr (initscr))
    ;; (raw!)
    ;; (keypad! stdscr #t)
    ;; (noecho!)

    ;; Xi init code
    (xile--msg-send port-to-xi (xile--msg-init '()))
    (xile--msg-send port-to-xi (xile--msg-new_view '((file_path . "README.org"))))

    ;; (addstr stdscr "Type any character to see it in bold\n")
    ;; (let ((ch (getch stdscr)))
    ;;   (addstr stdscr "The pressed key is ")
    ;;   (if (char? ch)
    ;;       (addch stdscr (bold ch))
    ;;       (addchstr stdscr (bold (keyname ch))))

    ;;   (refresh stdscr)
    ;;   (getch stdscr)
    ;;   (endwin))
    ;; TODO : event loop thread instead of joining
    (join-thread listener (+ 2 (current-time)))
    (close-port port-to-xi)
    (close-port port-from-xi)))
