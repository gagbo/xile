#!/usr/bin/env -S guile --no-auto-compile -s
!#

(load "../xile/message.scm")
(load "../xile/backend-notifications.scm")
(load "../xile/line-cache.scm")

(use-modules (srfi srfi-64)
             (xile line-cache)
             (xile backend-notifications))

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
  '(("op" . "copy") ("n" . 2) ("ln" . 1)))

(define test-skip-message
  '(("op" . "skip") ("n" . 1)))

(define test-invalid-xi-line
  (make-xi-line "" 0 #() #() #f))

(define test-empty-cache
  (make-xi-line-cache #() 0 0))

(define test-one-line-cache-all-valid
  (make-xi-line-cache #(test-ins-line-1) 0 1))

(define test-one-line-cache-some-valid
  (make-xi-line-cache #(test-invalid-xi-line test-ins-line-1 test-invalid-xi-line) 1 2))

(define (wrap-messages-in-update msg-list)
  (parse-xi-update `(("view_id" . "test-update-spec")
                     ("update" . (("ops" . ,(list->vector msg-list))
                                  ("rev" . 1)
                                  ("pristine" . #f))))))

(test-begin "update-operations")
(define insert-into-an-empty-cache
  (xi-line-cache-execute-update
   test-empty-cache
   (wrap-messages-in-update `(,test-ins-message))))

(test-equal "Insert into an empty cache -- invalid_before"
  (xi-line-cache-invalid_before insert-into-an-empty-cache)
  0)

(test-equal "Insert into an empty cache -- invalid_after"
  (xi-line-cache-invalid_after insert-into-an-empty-cache)
  3)

(test-equal "Insert into an empty cache -- valid range size"
  (vector-length (xi-line-cache-valid-range insert-into-an-empty-cache))
  3)

(test-equal "Insert into an empty cache -- first valid line"
  (xi-line-text (vector-ref (xi-line-cache-valid-range insert-into-an-empty-cache) 0))
  (assoc-ref test-ins-line-1 "text"))

(test-end "update-operations")

;; Make the exit code match the total number of failed tests for CI purposes.
(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
