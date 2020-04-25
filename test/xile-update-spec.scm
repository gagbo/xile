#!/usr/bin/env -S guile --no-auto-compile -s
!#

(load "../xile/line-cache.scm")

(use-modules (srfi srfi-64)
             (xile line-cache))


(test-begin "update-operations")

(test-end "update-operations")

;; Make the exit code match the total number of failed tests for CI purposes.
(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
