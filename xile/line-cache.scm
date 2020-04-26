;; coding: utf-8

(define-module (xile line-cache)
  #:use-module (xile message)
  #:use-module (xile backend-notifications)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (xi-line-cache-execute-update
            make-xi-line-cache
            xi-line-cache-invalid_before
            xi-line-cache-invalid_after
            xi-line-cache-lines
            xi-line-cache-valid-range))

(define-record-type <xi-line-cache>
  (make-xi-line-cache lines invalid_before invalid_after)
  xi-line-cache?
  (lines xi-line-cache-lines set-xi-line-cache-lines)
  (invalid_before xi-line-cache-invalid_before set-xi-line-cache-invalid_before)
  (invalid_after xi-line-cache-invalid_after set-xi-line-cache-invalid_after))

(define (xi-line-cache-valid-range cache)
  "Return a vector of xi-line elements in the invalid_before/invalid_after range of CACHE."
  (vector-copy
   (xi-line-cache-lines cache)
   (xi-line-cache-invalid_before cache)
   (xi-line-cache-invalid_after cache)))

(define (xi-line-cache-execute-update cache update)
  "Return a xi-line-cache with all UPDATE operations accomplished


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


An update request can be seen as a function from the old client
cache state to a new one. During evaluation, maintain an index
(old_ix) into the old lines array, initially 0, and a new lines
array, initially empty. [Note that this document specifies the
semantics. The actual implementation will almost certainly
represent at least initial and trailing sequences of invalid lines
by their count; and the editing operations may be more efficiently
done in-place than by copying from the old state to the new]. "

  (and=>
   (xi-update-ops update)
   (lambda (vec)
     (vector-fold (lambda (_ state op) (xi-line-cache-handle-op state op)) cache vec))))

(define (xi-line-cache-handle-op cache op)
  "Dispatcher for executing a single update OP on CACHE"
  (let ((type (xi-op-type op)))
    (cond
     ((eq? type 'ins)
      (let ((new-cache (xi-line-cache-handle-update-ins cache op)))
        (format (current-error-port) "After ins : ~%~y~%" new-cache)
        new-cache))
     ((eq? type 'invalidate)
      (let ((new-cache (xi-line-cache-handle-update-invalidate cache op)))
        (format (current-error-port) "After invalidate : ~%~y~%" new-cache)
        new-cache))
     ((eq? type 'copy)
      (let ((new-cache (xi-line-cache-handle-update-copy cache op)))
        (format (current-error-port) "After copy : ~%~y~%" new-cache)
        new-cache))
     ((eq? type 'skip)
      (let ((new-cache (xi-line-cache-handle-update-skip cache op)))
        (format (current-error-port) "After skip : ~%~y~%" new-cache)
        new-cache))
     ((eq? type 'update)
      (let ((new-cache (xi-line-cache-handle-update-update cache op)))
        (format (current-error-port) "After update : ~%~y~%" new-cache)
        new-cache))
     (else (format (current-error-port) "Unknown update type : ~a~%" type)
           cache))))

(define (xi-line-cache-handle-update-update cache update)
  "Execute the update operation UPDATE on CACHE.

The “update” op updates the cursor and/or style of n existing
lines. As in “ins”, n must equal lines.length. It also increments
old_ix by n.

Note: The “update” op is not currently used by core.
"
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
(old-lines (xi-line-cache-lines cache))
(count (xi-op-count update)))
    (make-xi-line-cache old-lines inv-before inv-after)))

(define (xi-line-cache-handle-update-copy cache update)
  "Execute the copy operation UPDATE on CACHE.

The “copy” op appends the n lines [old_ix .. old_ix + n] to the new
lines array, and increments old_ix by n. Additionally, “copy”
includes the ln field; this represents the new logical line number
(that is, the ‘real’ line number, ignoring word wrap) of the first
line to be copied. Note: if the first line to be copied is itself a
wrapped line, the ln number will need to be incremented in order to
be correct for the first ‘real’ line. "

  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
(old-lines (xi-line-cache-lines cache))
(count (xi-op-count update)))

    ;; Do not do anything if we're asked to copy everything
    (if (and (= 0 inv-before) (= count (vector-length old-lines)))
        cache
        (let* ((new-lines (make-vector (- inv-after inv-before))))
          (vector-copy! new-lines 0 old-lines (+ inv-before count) inv-after)
          (vector-copy! new-lines (- inv-after (+ inv-before count)) old-lines inv-before (+ inv-before count))
          (make-xi-line-cache new-lines 0 (- inv-after inv-before))))))

(define (xi-line-cache-handle-update-skip cache update)
  "Execute the skip operation UPDATE on CACHE.

The “skip” op increments old_ix by n. "
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
(old-lines (xi-line-cache-lines cache))
(count (xi-op-count update)))

    (define new-lines (vector-copy old-lines (+ count inv-before)))
    (make-xi-line-cache new-lines 0 (- inv-after count))))

(define (xi-line-cache-handle-update-invalidate cache update)
  "Execute the invalidate operation UPDATE on CACHE

The “invalidate” op appends n invalid lines to the new lines array. "
  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
(old-lines (xi-line-cache-lines cache))
(count (xi-op-count update)))

    (define new-lines (make-vector (+ (vector-length old-lines) count)))
    (vector-copy! new-lines 0 old-lines)

    (if (equal? (vector-length old-lines) 0)
        (make-xi-line-cache new-lines (+ inv-before count) inv-after)
        (make-xi-line-cache new-lines inv-before inv-after))))

(define (xi-line-cache-handle-update-ins cache update)
  "Execute the ins operation UPDATE on CACHE

The “ins” op appends new lines, specified by the “lines” parameter,
specified in more detail below. For this op, n must equal
lines.length (alternative: make n optional in this case). It does
not update old_ix. "

  (let ((inv-before (xi-line-cache-invalid_before cache))
        (inv-after (xi-line-cache-invalid_after cache))
(old-lines (xi-line-cache-lines cache))
(count (xi-op-count update))
(ins-lines (xi-op-lines update)))

    (define new-lines (make-vector (+ inv-after (vector-length ins-lines))))
    (vector-copy! new-lines 0 old-lines 0 inv-after)
    (vector-copy! new-lines inv-after ins-lines)

    (make-xi-line-cache new-lines inv-before (vector-length new-lines))))
