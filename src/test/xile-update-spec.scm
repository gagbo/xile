#!/usr/bin/env -S guile --no-auto-compile -s
!#

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (srfi srfi-64)
             (xile xi-protocol line-cache)
             (xile xi-protocol notification-types))

(define (wrap-messages-in-update msg-list)
  (parse-xi-update `(("view_id" . "test-update-spec")
                     ("update" . (("ops" . ,(list->vector msg-list))
                                  ("rev" . 1)
                                  ("pristine" . #f))))))

(define (wrap-alist-in-line single-line)
  (parse-xi-line single-line))

;; Testing variables
;; This will prevent having to write it in every test
(define test-ins-line-1
  '(("text" . "#+TITLE: [WIP] Xile : Xi + Guile\n")
    ("styles" . #())
    ("ln" . 1)
    ("cursor" . #(0))))

(define test-ins-line-2
  '(("text" . "\n") ("styles" . #()) ("ln" . 2)))

(define test-ins-line-3
  '(("text"
     .
     "Let's find out if Guile goes with everything ; and make a front-end for Xi\n")
    ("styles" . #())
    ("ln" . 3)))

(define test-ins-message
  `(("op" . "ins")
    ("n" . 3)
    ("lines"
     .
     #(,test-ins-line-1
       ,test-ins-line-2
       ,test-ins-line-3))))

(define test-invalidate-message
  '(("op" . "invalidate") ("n" . 40)))

(define test-copy-message
  '(("op" . "copy") ("n" . 1) ("ln" . 1)))

(define test-skip-message
  '(("op" . "skip") ("n" . 1)))

(define test-invalid-xi-line
  (make-xi-line "" 0 #() #() #f))

(define test-empty-cache
  (make-xi-line-cache #() 0 0))

(define test-one-line-cache-all-valid
  (make-xi-line-cache (vector (wrap-alist-in-line test-ins-line-1)) 0 1))

(define test-one-line-cache-some-valid
  (make-xi-line-cache (vector test-invalid-xi-line (wrap-alist-in-line test-ins-line-1) test-invalid-xi-line) 1 2))

(test-begin "update-operations")
;;; Insertion tests
;; Insertion into an empty cache
(define insert-into-an-empty-cache
  (xi-line-cache-execute-update
   test-empty-cache
   (wrap-messages-in-update `(,test-ins-message))))

(test-equal "Insert into an empty cache -- invalid_before"
  0
  (xi-line-cache-invalid_before insert-into-an-empty-cache))

(test-equal "Insert into an empty cache -- invalid_after"
  3
  (xi-line-cache-invalid_after insert-into-an-empty-cache))

(test-equal "Insert into an empty cache -- valid range size"
  3
  (vector-length (xi-line-cache-valid-range insert-into-an-empty-cache)))

(test-equal "Insert into an empty cache -- first valid line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range insert-into-an-empty-cache) 0)))

;; Insertion into a hot cache
(define insert-into-a-hot-cache
  (xi-line-cache-execute-update
   test-one-line-cache-all-valid
   (wrap-messages-in-update `(,test-ins-message))))

(test-equal "Insert into a hot cache -- invalid_before"
  0
  (xi-line-cache-invalid_before insert-into-a-hot-cache))

(test-equal "Insert into a hot cache -- invalid_after"
  4
  (xi-line-cache-invalid_after insert-into-a-hot-cache))

(test-equal "Insert into a hot cache -- valid range size"
  4
  (vector-length (xi-line-cache-valid-range insert-into-a-hot-cache)))

(test-equal "Insert into a hot cache -- second valid line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range insert-into-a-hot-cache) 1)))

;; Insertion into a dirty cache
(define insert-into-a-dirty-cache
  (xi-line-cache-execute-update
   test-one-line-cache-some-valid
   (wrap-messages-in-update `(,test-ins-message))))

(test-equal "Insert into a dirty cache -- invalid_before"
  1
  (xi-line-cache-invalid_before insert-into-a-dirty-cache))

(test-equal "Insert into a dirty cache -- invalid_after"
  5
  (xi-line-cache-invalid_after insert-into-a-dirty-cache))

(test-equal "Insert into a dirty cache -- valid range size"
  4
  (vector-length (xi-line-cache-valid-range insert-into-a-dirty-cache)))

(test-equal "Insert into a dirty cache -- last valid line"
  (assoc-ref test-ins-line-3 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range insert-into-a-dirty-cache) 3)))

;;; Invalidation tests
(define invalidate-into-an-empty-cache
  (xi-line-cache-execute-update
   test-empty-cache
   (wrap-messages-in-update `(,test-invalidate-message))))

(test-equal "Invalidate into an empty cache -- invalid_before"
  40
  (xi-line-cache-invalid_before invalidate-into-an-empty-cache))

(test-equal "Invalidate into an empty cache -- invalid_after"
  0
  (xi-line-cache-invalid_after invalidate-into-an-empty-cache))

(test-equal "Invalidate into an empty cache -- valid range size"
  0
  (vector-length (xi-line-cache-valid-range invalidate-into-an-empty-cache)))

(define invalidate-into-a-hot-cache
  (xi-line-cache-execute-update
   test-one-line-cache-all-valid
   (wrap-messages-in-update `(,test-invalidate-message))))

(test-equal "Invalidate into a hot cache -- invalid_before"
  0
  (xi-line-cache-invalid_before invalidate-into-a-hot-cache))

(test-equal "Invalidate into a hot cache -- invalid_after"
  1
  (xi-line-cache-invalid_after invalidate-into-a-hot-cache))

(test-equal "Invalidate into a hot cache -- valid range size"
  1
  (vector-length (xi-line-cache-valid-range invalidate-into-a-hot-cache)))

(test-equal "Invalidate into a hot cache -- second valid line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range invalidate-into-a-hot-cache) 0)))

(define invalidate-into-a-dirty-cache
  (xi-line-cache-execute-update
   test-one-line-cache-some-valid
   (wrap-messages-in-update `(,test-invalidate-message))))

(test-equal "Invalidate into a dirty cache -- invalid_before"
  1
  (xi-line-cache-invalid_before invalidate-into-a-dirty-cache))

(test-equal "Invalidate into a dirty cache -- invalid_after"
  2
  (xi-line-cache-invalid_after invalidate-into-a-dirty-cache))

(test-equal "Invalidate into a dirty cache -- valid range size"
  1
  (vector-length (xi-line-cache-valid-range invalidate-into-a-dirty-cache)))

(test-equal "Invalidate into a dirty cache -- last valid line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range invalidate-into-a-dirty-cache) 0)))

;;; Copy tests
(test-error "Copying from an empty cache is wrong." #t
  (xi-line-cache-execute-update
   test-empty-cache
   (wrap-messages-in-update `(,test-copy-message))))

(define copy-into-a-hot-cache
  (xi-line-cache-execute-update
   test-one-line-cache-all-valid
   (wrap-messages-in-update `(,test-copy-message))))

(test-equal "Copy into a hot cache -- invalid_before"
  0
  (xi-line-cache-invalid_before copy-into-a-hot-cache))

(test-equal "Copy into a hot cache -- invalid_after"
  1
  (xi-line-cache-invalid_after copy-into-a-hot-cache))

(test-equal "Copy into a hot cache -- valid range size"
  1
  (vector-length (xi-line-cache-valid-range copy-into-a-hot-cache)))

(test-equal "Copy into a hot cache -- Valid copied line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range copy-into-a-hot-cache) 0)))

(define copy-into-a-dirty-cache
  (xi-line-cache-execute-update
   test-one-line-cache-some-valid
   (wrap-messages-in-update `(,test-copy-message))))

(test-equal "Copy into a dirty cache -- invalid_before"
  0
  (xi-line-cache-invalid_before copy-into-a-dirty-cache))

(test-equal "Copy into a dirty cache -- invalid_after"
  1
  (xi-line-cache-invalid_after copy-into-a-dirty-cache))

(test-equal "Copy into a dirty cache -- valid range size"
  1
  (vector-length (xi-line-cache-valid-range copy-into-a-dirty-cache)))

(test-equal "Copy into a dirty cache -- Valid copied line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range copy-into-a-dirty-cache) 0)))


;;; Skip tests
(test-error "Skip operation on a empty cache is wrong" #t
  (xi-line-cache-execute-update
   test-empty-cache
   (wrap-messages-in-update `(,test-skip-message))))

(define skip-into-a-hot-cache
  (xi-line-cache-execute-update
   test-one-line-cache-all-valid
   (wrap-messages-in-update `(,test-skip-message))))

(test-equal "Skip into a hot cache -- invalid_before"
  0
  (xi-line-cache-invalid_before skip-into-a-hot-cache))

(test-equal "Skip into a hot cache -- invalid_after"
  0
  (xi-line-cache-invalid_after skip-into-a-hot-cache))

(test-equal "Skip into a hot cache -- valid range size"
  0
  (vector-length (xi-line-cache-valid-range skip-into-a-hot-cache)))

(define skip-into-a-dirty-cache
  (xi-line-cache-execute-update
   test-one-line-cache-some-valid
   (wrap-messages-in-update `(,test-skip-message))))

;; This test skips the first invalid line from the cache, and we are
;; back on the valid line.
(test-equal "Skip into a dirty cache -- invalid_before"
  0
  (xi-line-cache-invalid_before skip-into-a-dirty-cache))

(test-equal "Skip into a dirty cache -- invalid_after"
  1
  (xi-line-cache-invalid_after skip-into-a-dirty-cache))

(test-equal "Skip into a dirty cache -- valid range size"
  1
  (vector-length (xi-line-cache-valid-range skip-into-a-dirty-cache)))

(test-equal "Skip into a dirty cache -- Valid unskipped line"
  (assoc-ref test-ins-line-1 "text")
  (xi-line-text (vector-ref (xi-line-cache-valid-range copy-into-a-dirty-cache) 0)))

(test-end "update-operations")

;; Make the exit code match the total number of failed tests for CI purposes.
(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
