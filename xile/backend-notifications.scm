;; coding: utf-8

(define-module (xile backend-notifications)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (xile themes)
  #:export (parse-xi-update
            parse-xi-line
            parse-xi-op
            parse-xi-scroll-to
            parse-xi-measure-width
            parse-xi-theme-changed
            parse-xi-available-languages
            parse-xi-available-themes
            parse-xi-buffer-config-change
            xi-op?
            xi-op-type
            xi-op-count
            xi-op-lines
            xi-op-ln
            make-xi-line
            xi-line?
            xi-line-text
            xi-line-ln
            xi-line-cursor
            xi-line-styles
            xi-line-valid
            set-xi-line-valid
            xi-update?
            xi-update-rev
            xi-update-ops
            xi-update-view_id
            xi-update-pristine
            xi-update-annotations
            xi-scroll-to?
            make-xi-scroll-to
            xi-scroll-to-line
            xi-scroll-to-col
            xi-measure-width?
            make-xi-measure-width
            xi-measure-width-id
            xi-measure-width-strings
            xi-theme-changed?
            xi-theme-changed-name
            xi-theme-changed-settings
            xi-available-languages?
            xi-available-languages-list
            xi-available-themes?
            xi-available-themes-list
            xi-buffer-config-change?
            xi-buffer-config-change-wrap-width
            xi-buffer-config-change-word-wrap
            xi-buffer-config-change-use-tab-stops
            xi-buffer-config-change-translate-tabs-to-spaces
            xi-buffer-config-change-tab-size
            xi-buffer-config-change-surrounding-pairs
            xi-buffer-config-change-scroll-past-end
            xi-buffer-config-change-save-with-newline
            xi-buffer-config-change-plugin-search-path
            xi-buffer-config-change-line-ending
            xi-buffer-config-change-font-size
            xi-buffer-config-change-font-face
            xi-buffer-config-change-autodetect-whitespace))

(define-record-type <xi-op>
  (make-xi-op type count lines ln)
  xi-op?
  (type xi-op-type)
  (count xi-op-count)
  (lines xi-op-lines)
  (ln xi-op-ln))

(define-record-type <xi-line>
  (make-xi-line text ln cursor styles valid)
  xi-line?
  (text xi-line-text)
  (ln xi-line-ln)
  (cursor xi-line-cursor)
  (styles xi-line-styles)
  (valid xi-line-valid set-xi-line-valid))

(define-record-type <xi-annotation-slice>
  (make-xi-annotation-slice type ranges payloads count-ranges)
  xi-annotation-slice?
  (type xi-annotation-slice-type)
  (ranges xi-annotation-slice-ranges)
  (payloads xi-annotation-slice-payloads)
  (count-ranges xi-annotation-slice-count-ranges))

(define-record-type <xi-annotation-range>
  (make-xi-annotation-range start_line start_col end_line end_col)
  xi-annotation-range?
  (start_line xi-annotation-range-start_line)
  (start_col xi-annotation-range-start_col)
  (end_line xi-annotation-range-end_line)
  (end_col xi-annotation-range-end_col))

(define-record-type <xi-update>
  (make-xi-update rev ops view_id pristine annotations)
  xi-update?
  (rev xi-update-rev)
  (ops xi-update-ops)
  (view_id xi-update-view_id)
  (pristine xi-update-pristine)
  (annotations xi-update-annotations))

(define (parse-xi-line line)
  "Parse a deserialized Json LINE into a record.

interface Line {
  text?: string  // present when op is \"update\"
  ln?: number // the logical/'real' line number for this line.
  cursor?: number[]  // utf-8 code point offsets, in increasing order
  styles?: number[]  // length is a multiple of 3, see below
} "

  (let ((text (assoc-ref line "text"))
        (ln (assoc-ref line "ln"))
        (cursor (assoc-ref line "cursor"))
        (styles (assoc-ref line "styles")))
    (make-xi-line text ln cursor styles #t)))

(define (parse-xi-op op)
  (let ((type (string->symbol (assoc-ref op "op")))
        (count (assoc-ref op "n"))
        (lines (and=> (assoc-ref op "lines") (lambda (vec) (vector-map (lambda (i line) (parse-xi-line line)) vec))))
        (ln (assoc-ref op "ln")))
    (make-xi-op type count lines ln)))

(define (parse-xi-update result)
  "Parse a deserialized Json RESULT into an update

update
rev?: number
ops: Op[]
view-id: string
pristine: bool
annotations: AnnotationSlice[]

interface Op {
  op: \"copy\" | \"skip\" | \"invalidate\" | \"update\" | \"ins\"
  n: number  // number of lines affected
  lines?: Line[]  // only present when op is \"update\" or \"ins\"
  ln?: number // the logical number for this line; null if this line is a soft break
}

interface AnnotationSlice {
  type: \"find\" | \"selection\" | ...
  ranges: [[number, number, number, number]]  // start_line, start_col, end_line, end_col
  payloads: [{}]    // can be any json object or value
  n: number // number of ranges
}

The pristine flag indicates whether or not, after this update, this
document has unsaved changes.

The rev field is not present in current builds, but will be at some
point in the future. "
 
  (let ((view_id (assoc-ref result "view_id"))
        (update-alist (assoc-ref result "update")))
    (let ((ops (and=> (assoc-ref update-alist "ops") (lambda (vec) (vector-map (lambda (i op) (parse-xi-op op)) vec))))
          (rev (assoc-ref update-alist "rev"))
          (pristine (assoc-ref update-alist "pristine"))
          (annotations #f))
      (make-xi-update rev ops view_id pristine annotations))))


;; The interpretation of a line is different for “update” or “ins”
;; ops. In an “ins” op, text is always present, and missing cursor or
;; styles properties are interpreted as empty (no cursors on that
;; line, no styles).

;; In an “update” op, then the text property is absent from the line,
;; and text is copied from the previous state (or left invalid if the
;; previous state is invalid), and the cursor and styles are updated
;; if present. To delete cursors from a line, the core sets the cursor
;; property to the empty list.

;; The styles property represents style spans, in an efficient
;; encoding. It is conceptually an array of triples (though flattened,
;; so triple at is styles[i*3], styles[i*3 + 1], styles[i*3 + 2]). The
;; first element of the triple is the start index (in utf-8 code
;; units), but encoded as a delta relative to the end of the last span
;; (or relative to 0 for the first triple). It may be negative, if
;; spans overlap. The second element is the length (in utf-8 code
;; units). It is guaranteed nonzero and positive. The third element is
;; a style id. The core guarantees that any style id sent in a styles
;; property will have previously been set in a set_style request.

;; The number of lines in the new lines array always matches the view
;; as maintained by the core. Another way of saying this is that
;; adding all “n” values except for “skip” operations is the number of
;; lines. [Discussion: the last line always represents a partial line,
;; so an empty document is one empty line. But I think the initial
;; state should be the empty array. Then, the empty array represents
;; the state that no updates have been processed].

;; interface Line {
;;   text?: string  // present when op is "update"
;;   cursor?: number[]  // utf-8 code point offsets, in increasing order
;;   styles?: number[]  // length is a multiple of 3, see below
;; }

;; “annotations” are used to associate some type data with some
;; document regions. For example, annotations are used to represent
;; selections and find highlights. The Annotations RFC provides a
;; detailed description of the API.

(define-record-type <xi-scroll-to>
  (make-xi-scroll-to line col)
  xi-scroll-to?
  (line xi-scroll-to-line)
  (col xi-scroll-to-col))

(define (parse-xi-scroll-to result)
  "Parse a deserialize json RESULT into a xi-scroll-to record

scroll_to: {\"line\": number, \"col\": number} // line, column (in utf-8 code units) "
  (let ((line (assoc-ref result "line"))
        (col (assoc-ref result "col")))
    (make-xi-scroll-to line col)))

(define-record-type <xi-measure-width>
  (make-xi-measure-width id strings)
  xi-measure-width?
  (id xi-measure-width-id)
  (strings xi-measure-width-strings))

(define (parse-xi-measure-width result)
  "Parse a deserialized json RESULT into a xi-measure-width request record

measure_width [{\"id\": number, \"strings\": string[]}] <- {\"id\":0, \"result\":[[28.0,8.0]]} "
  (let ((id (assoc-ref result "id"))
        (strings (assoc-ref result "strings")))
    (make-xi-measure-width id strings)))

(define-record-type <xi-theme-changed>
  (make-xi-theme-changed name settings)
  xi-theme-changed?
  (name xi-theme-changed-name)
  (settings xi-theme-changed-settings))

(define (parse-xi-theme-changed result)
  "Parse a deserialized json RESULT into a xi-theme-changed record.

theme_changed {\"name\": \"InspiredGitHub\", \"theme\": Theme}
The Theme object is directly serialized from a syntect::highlighting::ThemeSettings instance. "
  (let ((name (assoc-ref result "name"))
        (settings (parse-syntect-theme-settings (assoc-ref result "theme"))))
    (make-xi-theme-changed name settings)))

(define-record-type <xi-available-languages>
  (make-xi-available-languages languages)
  xi-available-languages?
  (languages xi-available-languages-list))

(define (parse-xi-available-languages result)
  "Parse a deserialized json RESULT into a xi-available-languages record.

available_languages {\"languages\": [\"Rust\"]}
"
  (make-xi-available-languages (or (assoc-ref result "languages") #())))

(define-record-type <xi-available-themes>
  (make-xi-available-themes theme-list)
  xi-available-themes?
  (theme-list xi-available-themes-list))

(define (parse-xi-available-themes result)
  "Parse a deserialized json RESULT into a xi-available-themes record.

available_themes {\"themes\": [\"InspiredGitHub\"]}"
  (make-xi-available-themes (or (assoc-ref result "themes") #())))

(define-record-type <xi-buffer-config-change>
  (make-xi-buffer-config-change
   wrap-width word-wrap use-tab-stops
   translate-tabs-to-spaces tab-size surrounding-pairs
   scroll-past-end save-with-newline plugin-search-path
   line-ending font-size font-face
   autodetect-whitespace auto-indent)
  xi-buffer-config-change?
  (wrap-width xi-buffer-config-change-wrap-width)
  (word-wrap xi-buffer-config-change-word-wrap)
  (use-tab-stops xi-buffer-config-change-use-tab-stops)
  (translate-tabs-to-spaces xi-buffer-config-change-translate-tabs-to-spaces)
  (tab-size xi-buffer-config-change-tab-size)
  (surrounding-pairs xi-buffer-config-change-surrounding-pairs)
  (scroll-past-end xi-buffer-config-change-scroll-past-end)
  (save-with-newline xi-buffer-config-change-save-with-newline)
  (plugin-search-path xi-buffer-config-change-plugin-search-path)
  (line-ending xi-buffer-config-change-line-ending)
  (font-size xi-buffer-config-change-font-size)
  (font-face xi-buffer-config-change-font-face)
  (autodetect-whitespace xi-buffer-config-change-autodetect-whitespace)
  (auto-indent xi-buffer-config-change-auto-indent))

(define (parse-xi-buffer-config-change result)
  "Parse a deserialized json RESULT into a xi-buffer-config-change record."
  (make-xi-buffer-config-change
   (assoc-ref result "wrap_width")
   (assoc-ref result "word_wrap")
   (assoc-ref result "use_tab_stops")
   (assoc-ref result "translate_tabs_to_spaces")
   (assoc-ref result "tab_size")
   (assoc-ref result "surrounding_pairs")
   (assoc-ref result "scroll_past_end")
   (assoc-ref result "save_with_newline")
   (assoc-ref result "plugin_search_path")
   (assoc-ref result "line_ending")
   (assoc-ref result "font_size")
   (assoc-ref result "font_face")
   (assoc-ref result "autodetect_whitespace")
   (assoc-ref result "auto_indent")))
