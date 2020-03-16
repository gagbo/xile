;; coding: utf-8

(define-module (xile backend-notifications)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (parse-xi-update
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
            xi-update-annotations))

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
  (let ((view_id (assoc-ref result "view_id"))
        (update-alist (assoc-ref result "update")))
    (let ((ops (and=> (assoc-ref update-alist "ops") (lambda (vec) (vector-map (lambda (i op) (parse-xi-op op)) vec))))
          (rev (assoc-ref update-alist "rev"))
          (pristine (assoc-ref update-alist "pristine"))
          (annotations #f))
      (make-xi-update rev ops view_id pristine annotations))))

;; update
;; rev?: number
;; ops: Op[]
;; view-id: string
;; pristine: bool
;; annotations: AnnotationSlice[]

;; interface Op {
;;   op: "copy" | "skip" | "invalidate" | "update" | "ins"
;;   n: number  // number of lines affected
;;   lines?: Line[]  // only present when op is "update" or "ins"
;;   ln?: number // the logical number for this line; null if this line is a soft break
;; }

;; interface AnnotationSlice {
;;   type: "find" | "selection" | ...
;;   ranges: [[number, number, number, number]]  // start_line, start_col, end_line, end_col
;;   payloads: [{}]    // can be any json object or value
;;   n: number // number of ranges
;; }

;; The pristine flag indicates whether or not, after this update, this
;; document has unsaved changes.

;; The rev field is not present in current builds, but will be at some
;; point in the future.

;; An update request can be seen as a function from the old client
;; cache state to a new one. During evaluation, maintain an index
;; (old_ix) into the old lines array, initially 0, and a new lines
;; array, initially empty. [Note that this document specifies the
;; semantics. The actual implementation will almost certainly
;; represent at least initial and trailing sequences of invalid lines
;; by their count; and the editing operations may be more efficiently
;; done in-place than by copying from the old state to the new].

;; The “copy” op appends the n lines [old_ix .. old_ix + n] to the new
;; lines array, and increments old_ix by n. Additionally, “copy”
;; includes the ln field; this represents the new logical line number
;; (that is, the ‘real’ line number, ignoring word wrap) of the first
;; line to be copied. Note: if the first line to be copied is itself a
;; wrapped line, the ln number will need to be incremented in order to
;; be correct for the first ‘real’ line.

;; The “skip” op increments old_ix by n.

;; The “invalidate” op appends n invalid lines to the new lines array.

;; The “ins” op appends new lines, specified by the “lines” parameter,
;; specified in more detail below. For this op, n must equal
;; lines.length (alternative: make n optional in this case). It does
;; not update old_ix.

;; The “update” op updates the cursor and/or style of n existing
;; lines. As in “ins”, n must equal lines.length. It also increments
;; old_ix by n.

;; Note: The “update” op is not currently used by core.

;; In all cases, n is guaranteed positive and nonzero (as a
;; consequence, any line present in the old state is copied at most
;; once to the new state).

;; interface Line {
;;   text?: string  // present when op is "update"
;;   ln?: number // the logical/'real' line number for this line.
;;   cursor?: number[]  // utf-8 code point offsets, in increasing order
;;   styles?: number[]  // length is a multiple of 3, see below
;; }

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
