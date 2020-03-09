;; coding: utf-8


(define-module (xile message)
  #:use-module (json)
  #:export (xile-msg-init
            xile-msg-new_view
            xile-msg-close_view
            xile-msg-save
            xile-msg-set_theme
            xile-msg-set_language
            xile-msg-modify_user_config
            xile-msg-get_config
            xile-msg-edit-insert
            xile-msg-edit-paste
            xile-msg-edit-copy
            xile-msg-edit-cut
            xile-msg-edit-scroll
            xile-msg-edit-resize
            xile-msg-edit-click
            xile-msg-edit-drag
            xile-msg-edit-gesture
            xile-msg-edit-goto_line
            xile-msg-edit-delete_backward
            xile-msg-edit-delete_forward
            xile-msg-edit-insert_newline
            xile-msg-edit-duplicate_line
            xile-msg-edit-move_up
            xile-msg-edit-move_up_and_modify_selection
            xile-msg-edit-move_down
            xile-msg-edit-move_down_and_modify_selection
            xile-msg-edit-move_left
            xile-msg-edit-move_left_and_modify_selection
            xile-msg-edit-move_right
            xile-msg-edit-move_right_and_modify_selection
            xile-msg-edit-scroll_page_up
            xile-msg-edit-page_up_and_modify_selection
            xile-msg-edit-scroll_page_down
            xile-msg-edit-page_down_and_modify_selection
            xile-msg-edit-yank
            xile-msg-edit-transpose
            xile-msg-edit-select_all
            xile-msg-edit-collapse_selections
            xile-msg-edit-add_selection_above
            xile-msg-edit-add_selection_below
            xile-msg-edit-uppercase
            xile-msg-edit-lowercase
            xile-msg-edit-capitalize
            xile-msg-edit-indent
            xile-msg-edit-outdent
            xile-msg-edit-increase_number
            xile-msg-edit-decrease_number
            xile-msg-edit-toggle_recording
            xile-msg-edit-play_recording
            xile-msg-edit-clear_recording
            xile-msg-edit-request_hover
            xile-msg-plugin-start
            xile-msg-plugin-stop
            xile-msg-plugin-rpc
            xile-msg-plugin-find
            xile-msg-plugin-multi_find
            xile-msg-plugin-find_next
            xile-msg-plugin-find_previous
            xile-msg-plugin-find_all
            xile-msg-plugin-highlight_find
            xile-msg-plugin-selection_for_find
            xile-msg-plugin-replace
            xile-msg-plugin-selection_for_replace
            xile-msg-plugin-replace_next
            xile-msg-plugin-replace_all
            xile-msg-plugin-selection_into_lines))


(define* (xile-notif-generic method #:key (notif-param-list '()))
  (cons #f (scm->json-string `((method . ,method) (params . ,notif-param-list)))))

(define xile-msg-generic #f)
(let ((id 0))
  (set! xile-msg-generic (lambda* (method #:key (msg-param-list '()))
                           (set! id (1+ id))
                           (cons id (scm->json-string `((id . ,id) (method . ,method) (params . ,msg-param-list)))))))

;; TODO : namespace should be a string or a symbol
(define* (xile-msg-namespace-generic namespace method #:key (ns-param-list '()))
  (xile-msg-generic namespace #:msg-param-list (acons 'method method ns-param-list))
      )

(define* (xile-msg-init #:key (config_dir #f) (client_extras_dir #f))
  "client_started {\"config_dir\" \"some/path\"?, \"client_extras_dir\": \"some/other/path\"?}

Sent by the client immediately after establishing the core connection.
This is used to perform initial setup. The two arguments are optional;
the config_dir points to a directory where the user’s config files and plugins live,
and the client_extras_dir points to a directory where the frontend can package
additional resources, such as bundled plugins."
  (xile-notif-generic
   'client_started
   #:notif-param-list (cond ((and config_dir client_extras_dir)
                             `((config_dir . ,config_dir) (client_extras_dir . ,client_extras_dir)))
                            (config_dir
                             `((config_dir . ,config_dir)))
                            (client_extras_dir
                             `((client_extras_dir . ,client_extras_dir)))
                            (else '()))))

(define* (xile-msg-new_view #:key (file_path #f))
  "new_view { \"file_path\": \"path.md\"? } -> \"view-id-1\"

Creates a new view, returning the view identifier as a string. file_path is optional;
if specified, the file is loaded into a new buffer; if not a new empty buffer is created.
Currently, only a single view into a given file can be open at a time.
Note:, there is currently no mechanism for reporting errors. Also note,
the protocol delegates power to load and save arbitrary files.
Thus, exposing the protocol to any other agent than a front-end in direct control
should be done with extreme caution.

Response from manual testing : ((result . view-id-1) (id . 1))"
  (if file_path
      (xile-msg-generic 'new_view #:msg-param-list `((file_path . ,file_path)))
      (xile-msg-generic 'new_view)))

(define* (xile-msg-close_view #:key (view_id "view-id-1"))
  "close_view {\"VIEW_ID\": \"view-id-1\"}

Closes the view associated with this view_id."
  (xile-msg-generic 'close_view #:msg-param-list `((view_id . ,view_id))))

(define* (xile-msg-save #:key (view_id "view-id-1") (file_path "test.txt"))
  "save {\"view_id\": \"view-id-4\", \"file_path\": \"save.txt\"}

Saves the buffer associated with view_id to file_path.
See the note for new_view. Errors are not currently reported."
  (xile-msg-generic 'save #:msg-param-list `((view_id . ,view_id) (file_path . ,file_path))))

(define* (xile-msg-set_theme #:key (theme_name "InspiredGitHub"))
  "set_theme {\"theme_name\": \"InspiredGitHub\"}

Asks core to change the theme. If the change succeeds the client will receive
a theme_changed notification."
  (xile-msg-generic 'set_theme #:msg-param-list `((theme_name . ,theme_name))))

(define* (xile-msg-set_language #:key (view_id "view-id-1") (language_id "Scheme"))
  "set_language {\"view-id\":\"view-id-1\", \"language_id\":\"Rust\"}

Asks core to change the language of the buffer associated with the view_id.
If the change succeeds the client will receive a language_changed notification."
  (xile-msg-generic 'set_language #:msg-param-list `((view_id . ,view_id) (language_id . ,language_id))))

(define* (xile-msg-modify_user_config #:key (domain "general") (changes '()))
  "modify_user_config { \"domain\": Domain, \"changes\": Object }

Modifies the user’s config settings for the given domain.
Domain should be either the string \"general\" or an object of the form
{\"syntax\": \"rust\"}, or {\"user_override\": \"view-id-1\"},
where \"rust\" is any valid syntax identifier, and \"view-id-1\" is
the identifier of any open view.

The object form is given in an alist."
  (xile-msg-generic 'modify_user_config #:msg-param-list `((domain . ,domain) (changes . ,changes))))

(define* (xile-msg-get_config #:key (view_id "view-id-1"))
  "get_config {\"view_id\": \"view-id-1\"} -> Object

Returns the config table for the view associated with this view_id."
  (xile-msg-generic 'get_config #:msg-param-list `((view_id . ,view_id))))

(define* (xile-msg-edit-generic method view_id #:key (edit-param-list '()))
  "edit {\"method\": \"insert\", \"params\": {\"chars\": \"A\"}, \"view_id\": \"view-id-4\"}

Dispatches the inner method to the per-tab handler,
with individual inner methods used later"
  (xile-msg-namespace-generic 'edit method #:ns-param-list `((view_id . ,view_id) (params . ,edit-param-list))))

(define (xile-msg-edit-insert view_id chars)
  "insert {\"chars\":\"A\"}

Inserts the chars string at the current cursor locations."
  (xile-msg-edit-generic 'insert view_id #:edit-param-list `((chars . ,chars))))

(define* (xile-msg-edit-paste view_id chars)
  "paste {\"chars\": \"password\"}

Inserts the chars string at the current cursor locations. If there are
multiple cursors and chars has the same number of lines as there are cursors,
one line will be inserted at each cursor, in order; otherwise the full string
will be inserted at each cursor."
  (xile-msg-edit-generic 'paste view_id #:edit-param-list `((chars . ,chars))))

(define (xile-msg-edit-copy view_id)
  "copy -> String|Null

Copies the active selection, returning their contents or Null if the
selection was empty."
  (xile-msg-edit-generic 'copy view_id))

(define (xile-msg-edit-cut view_id)
  "cut -> String|Null

Cut the active selection, returning their contents or Null if the selection
was empty"
  (xile-msg-edit-generic 'cut view_id))

(define (xile-msg-edit-scroll view_id start end)
  "scroll [0,18]

Notifies the back-end of the visible scroll region, defined as the first and
last (non-inclusive) formatted lines. The visible scroll region is used to
compute movement distance for page up and page down commands, and also
controls the size of the fragment sent in the update method."

  ;; TODO : notification, not method
  ;; (xile-msg-edit-generic 'scroll view_id #(start end))
  (xile-notif-generic
   'edit
   #:notif-param-list `((method . scroll) (view_id . ,view_id) (params . #(,start ,end)))))

(define (xile-msg-edit-resize view_id width height)
  "resize {width: 420, height: 400}

Notifies the backend that the size of the view has changed. This is used for
word wrapping, if enabled. Width and height are specified in px units /
points, not display pixels."
  (xile-msg-edit-generic 'resize view_id #:edit-param-list `((width . ,width) (height . ,height))))

(define (xile-msg-edit-click view_id line column modifiers count)
  "click [42,31,0,1]

Implements a mouse click. The array arguments are: line and column (0-based,
utf-8 code units), modifiers (again, 2 is shift), and click count."
  ;; (xile-msg-edit-generic 'click view_id #(line column modifiers count))
  (xile-notif-generic
   'edit
   #:notif-param-list `((method . click) (view_id . ,view_id) (params . #(,line ,column ,modifiers ,count)))))

(define (xile-msg-edit-drag view_id line column flag)
  "drag [42,32,0]

Implements dragging (extending a selection). Arguments are line, column, and
flag as in click."
  ;; (xile-msg-edit-generic 'drag view_id #(line column flag))
  (xile-notif-generic
   'edit
   #:notif-param-list `((method . drag) (view_id . ,view_id) (params . #(,line ,column ,flag)))))

(define (xile-msg-edit-gesture view_id gesture line column)
  "gesture {\"line\": 42, \"col\": 31, \"ty\": \"toggle_sel\"}

Note: both click and drag functionality will be migrated to additional ty options for gesture.

Currently, the following gestures are supported:

point_select # moves the cursor to a point
toggle_sel # adds or removes a selection at a point
range_select # modifies the selection to include a point (shift+click)
line_select # sets the selection to a given line
word_select # sets the selection to a given word
multi_line_select # adds a line to the selection
multi_word_select # adds a word to the selection"
  (xile-msg-edit-generic 'gesture view_id #:edit-param-list `((ty . ,gesture) (line . ,line) (col . ,column))))

(define (xile-msg-edit-goto_line view_id line)
  "goto_line {\"line\": 1}

Sets the cursor to the beginning of the provided line and scrolls to this
position."
  (xile-msg-edit-generic 'goto_line view_id #:edit-param-list `((line . ,line))))

;; Other movement and deletion commands
;;
;; The following edit methods take no parameters, and have similar meanings as
;; NSView actions. The pure movement and selection modification methods will be
;; migrated to a more general method that takes a “movement” enum as a
;; parameter.

(define (xile-msg-edit-delete_backward view_id)
  "delete_backward"
  (xile-msg-edit-generic 'delete_backward view_id))

(define (xile-msg-edit-delete_forward view_id)
  "delete_forward"
  (xile-msg-edit-generic 'delete_forward view_id))

(define (xile-msg-edit-insert_newline view_id)
  "insert_newline"
  (xile-msg-edit-generic 'insert_newline view_id))

(define (xile-msg-edit-duplicate_line view_id)
  "duplicate_line"
  (xile-msg-edit-generic 'duplicate_line view_id))

(define (xile-msg-edit-move_up view_id)
  "move_up"
  (xile-msg-edit-generic 'move_up view_id))

(define (xile-msg-edit-move_up_and_modify_selection view_id)
  "move_up_and_modify_selection"
  (xile-msg-edit-generic 'move_up_and_modify_selection view_id))

(define (xile-msg-edit-move_down view_id)
  "move_down"
  (xile-msg-edit-generic 'move_down view_id))

(define (xile-msg-edit-move_down_and_modify_selection view_id)
  "move_down_and_modify_selection"
  (xile-msg-edit-generic 'move_down_and_modify_selection view_id))

(define (xile-msg-edit-move_left view_id)
  "move_left"
  (xile-msg-edit-generic 'move_left view_id))

(define (xile-msg-edit-move_left_and_modify_selection view_id)
  "move_left_and_modify_selection"
  (xile-msg-edit-generic 'move_left_and_modify_selection view_id))

(define (xile-msg-edit-move_right view_id)
  "move_right"
  (xile-msg-edit-generic 'move_right view_id))

(define (xile-msg-edit-move_right_and_modify_selection view_id)
  "move_right_and_modify_selection"
  (xile-msg-edit-generic 'move_right_and_modify_selection view_id))

(define (xile-msg-edit-scroll_page_up view_id)
  "scroll_page_up"
  (xile-msg-edit-generic 'scroll_page_up view_id))

(define (xile-msg-edit-page_up_and_modify_selection view_id)
  "page_up_and_modify_selection"
  (xile-msg-edit-generic 'page_up_and_modify_selection view_id))

(define (xile-msg-edit-scroll_page_down view_id)
  "scroll_page_down"
  (xile-msg-edit-generic 'scroll_page_down view_id))

(define (xile-msg-edit-page_down_and_modify_selection view_id)
  "page_down_and_modify_selection"
  (xile-msg-edit-generic 'page_down_and_modify_selection view_id))

(define (xile-msg-edit-yank view_id)
  "yank"
  (xile-msg-edit-generic 'yank view_id))

(define (xile-msg-edit-transpose view_id)
  "transpose"
  (xile-msg-edit-generic 'transpose view_id))

(define (xile-msg-edit-select_all view_id)
  "select_all"
  (xile-msg-edit-generic 'select_all view_id))

(define (xile-msg-edit-collapse_selections view_id)
  "collapse_selections"
  (xile-msg-edit-generic 'collapse_selections view_id))

(define (xile-msg-edit-add_selection_above view_id)
  "add_selection_above"
  (xile-msg-edit-generic 'add_selection_above view_id))

(define (xile-msg-edit-add_selection_below view_id)
  "add_selection_below"
  (xile-msg-edit-generic 'add_selection_below view_id))

;; Transformations
;;
;; The following methods act by modifying the current selection.

(define (xile-msg-edit-uppercase view_id)
  "uppercase current selection"
  (xile-msg-edit-generic 'uppercase view_id))

(define (xile-msg-edit-lowercase view_id)
  "lowercase current selection"
  (xile-msg-edit-generic 'lowercase view_id))

(define (xile-msg-edit-capitalize view_id)
  "capitalize current selection"
  (xile-msg-edit-generic 'capitalize view_id))

(define (xile-msg-edit-indent view_id)
  "indent current selection"
  (xile-msg-edit-generic 'indent view_id))

(define (xile-msg-edit-outdent view_id)
  "outdent current selection"
  (xile-msg-edit-generic 'outdent view_id))

;; Number Transformations
;;
;; The following methods work with a caret or multiple selections. If the
;; beginning of a selection (or the caret) is within a positive or negative
;; number, the number will be transformed accordingly:

(define (xile-msg-edit-increase_number view_id)
  "increase_number"
  (xile-msg-edit-generic 'increase_number view_id))

(define (xile-msg-edit-decrease_number view_id)
  "decrease_number"
  (xile-msg-edit-generic 'decrease_number view_id))

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
(define (xile-msg-edit-toggle_recording view_id record_name)
  (xile-msg-edit-generic 'toggle_recording view_id #:edit-param-list `((recording_name . ,record_name))))

(define (xile-msg-edit-play_recording view_id record_name)
  (xile-msg-edit-generic 'play_recording view_id #:edit-param-list `((recording_name . ,record_name))))

(define (xile-msg-edit-clear_recording view_id record_name)
  (xile-msg-edit-generic 'clear_recording view_id #:edit-param-list `((recording_name . ,record_name))))

;; LSP stuff in edit namespace

(define (xile-msg-edit-request_hover view_id req_id line column)
  "Hover

Get Hover for a position in file. The request for hover is made as a
notification. The client is forwarded result back via a show_hover rpc

If position is skipped in the request, current cursor position will be used
in core."
  (xile-msg-edit-generic 'request_hover view_id #:edit-param-list  `((request_id . ,req_id) (position . ((line . ,line) (column . ,column))))))

(define* (xile-msg-plugin-generic method view_id #:key (plugin-param-list '()))
  "Plugin namespace

Note: plugin commands are in flux, and may change.

Example: The following RPC dispatches the inner method to the plugin manager.

plugin {\"method\": \"start\", params: {\"view_id\": \"view-id-1\", plugin_name: \"syntect\"}}"
  (xile-msg-namespace-generic 'plugin method #:ns-param-list `((params . ,(acons 'view_id view_id plugin-param-list)))))

(define (xile-msg-plugin-start view_id plugin_name)
  "Starts the named plugin for the given view."
  (xile-msg-plugin-generic 'start view_id #:plugin-param-list `((plugin_name . ,plugin_name))))

(define (xile-msg-plugin-stop view_id plugin_name)
  "Stops the named plugin for the given view."
  (xile-msg-plugin-generic 'stop view_id #:plugin-param-list `((plugin_name . ,plugin_name))))

(define (xile-msg-plugin-rpc view_id plugin_name request)
  "Sends a custom rpc command to the named receiver. This may be a notification
or a request.

plugin_rpc {\"view_id\": \"view-id-1\", \"receiver\": \"syntect\",
\"notification\": {
    \"method\": \"custom_method\",
    \"params\": {\"foo\": \"bar\"},
}}

In that example, REQUEST is
'((notification .
    ((method . \"custom_method\")
     (params .
        ((foo . \"bar\"))))))"
  (xile-msg-plugin-generic 'plugin_rpc view_id #:plugin-param-list `((receiver . ,plugin_name) ,request)))

(define* (xile-msg-plugin-find view_id chars case_sen #:optional (regex #f) (whole_words #f))
  "find {\"chars\": \"a\", \"case_sensitive\": false, \"regex\": false, \"whole_words\":
true} Parameters regex and whole_words are optional and by default false.

Sets the current search query and options."
  (xile-msg-plugin-generic 'find view_id #:plugin-param-list `((chars . ,chars) (case_sensitive . ,case_sen) (regex . ,regex) (whole_words . ,whole_words))))

(define* (xile-msg-plugin-multi_find view_id id chars case_sen #:optional (regex #f) (whole_words #f))
  "This find command supports multiple search queries.

multi_find [{\"id\": 1, \"chars\": \"a\", \"case_sensitive\": false, \"regex\": false,
\"whole_words\": true}] Parameters regex and whole_words are optional and by
default false. id is an optional parameter used to uniquely identify a search
query. If left empty, the query is considered as a new query and the backend
will generate a new ID.

Sets the current search queries and options."
  (xile-msg-plugin-generic 'multi_find view_id #:plugin-param-list `((id . ,id) (chars . ,chars) (case_sensitive . ,case_sen) (regex . ,regex) (whole_words . ,whole_words))))

(define* (xile-msg-plugin-find_next view_id #:optional (wrap_around #f) (allow_same #f) (modify_selection "set"))
  "find_next {\"wrap_around\": true, \"allow_same\": false, \"modify_selection\": \"set\"}

All parameters are optional. Boolean parameters are by default false and
modify_selection is set by default. If allow_same is set to true the current
selection is considered a valid next occurrence. Supported options for
modify_selection are:

none: the selection is not modified
set: the next/previous match will be set as the new selection
add: the next/previous match will be added to the current selection
add_removing_current: the previously added selection will be removed and the
  next/previous match will be added to the current selection

Selects the next/previous occurrence matching the search query."
  (xile-msg-plugin-generic 'find_next view_id #:plugin-param-list `((wrap_around . ,wrap_around) (allow_same . ,allow_same) (modify_selection . ,modify_selection))))

(define* (xile-msg-plugin-find_previous view_id #:optional (wrap_around #f) (allow_same #f) (modify_selection "set"))
  "find_previous {\"wrap_around\": true, \"allow_same\": false, \"modify_selection\": \"set\"}

All parameters are optional. Boolean parameters are by default false and
modify_selection is set by default. If allow_same is set to true the current
selection is considered a valid next occurrence. Supported options for
modify_selection are:

none: the selection is not modified
set: the next/previous match will be set as the new selection
add: the next/previous match will be added to the current selection
add_removing_current: the previously added selection will be removed and the
  next/previous match will be added to the current selection

Selects the next/previous occurrence matching the search query."
  (xile-msg-plugin-generic 'find_previous view_id #:plugin-param-list `((wrap_around . ,wrap_around) (allow_same . ,allow_same) (modify_selection . ,modify_selection))))

(define (xile-msg-plugin-find_all view_id)
  "find_all { }

Selects all occurrences matching the search query."
  (xile-msg-plugin-generic 'find_all view_id))

(define* (xile-msg-plugin-highlight_find view_id #:optional (visible #t))
  "highlight_find {\"visible\": true}

Shows/hides active search highlights."
  (xile-msg-plugin-generic 'highlight_find view_id #:plugin-param-list `((visible . ,visible))))

(define* (xile-msg-plugin-selection_for_find view_id #:optional (case_sen #f))
  "selection_for_find {\"case_sensitive\": false}
The parameter case_sensitive is optional and false if not set.

Sets the current selection as the search query."
  (xile-msg-plugin-generic 'selection_for_find view_id #:plugin-param-list `((case_sensitive . ,case_sen))))

(define* (xile-msg-plugin-replace view_id chars #:optional (preserve_case #f))
  "replace {\"chars\": \"a\", \"preserve_case\": false}
The parameter preserve_case is currently not implemented and ignored.

Sets the replacement string."
  (xile-msg-plugin-generic 'replace view_id #:plugin-param-list `((chars . ,chars) (preserve_case . ,preserve_case))))

(define* (xile-msg-plugin-selection_for_replace view_id #:optional (case_sen #f))
  "selection_for_replace {\"case_sensitive\": false}
The parameter case_sensitive is optional and false if not set.

Sets the current selection as the replacement string."
  (xile-msg-plugin-generic 'selection_for_replace view_id #:plugin-param-list `((case_sensitive . ,case_sen))))

(define (xile-msg-plugin-replace_next view_id)
  "replace_next { }

Replaces the next matching occurrence with the replacement string."
  (xile-msg-plugin-generic 'replace_next view_id))

(define (xile-msg-plugin-replace_all view_id)
  "replace_all { }

Replaces all matching occurrences with the replacement string."
  (xile-msg-plugin-generic 'replace_all view_id))

(define (xile-msg-plugin-selection_into_lines view_id)
  "selection_into_lines { }

Splits all current selections into lines."
  (xile-msg-plugin-generic 'selection_into_lines view_id))
