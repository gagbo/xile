;; coding: utf-8

(define-module (xile backend-notifications)
  #:use-module (srfi srfi-9)
  )

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
