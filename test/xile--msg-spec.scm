#!/usr/bin/env -S guile --no-auto-compile -s
!#

(load "check.scm")
(load "../xile.scm")

(use-modules (check)
             (ice-9 pretty-print))
(define test-errors '())

;;; xile : Messages Tests
;; Note that the values of id increment on real messages, starting from 1
(check (cdr (xile--msg-init '())) => "{\"method\":\"client_started\",\"params\":{}}" )
(check (cdr (xile--msg-init '((config_dir . "test_path/togo")))) => "{\"method\":\"client_started\",\"params\":{\"config_dir\":\"test_path/togo\"}}" )

(check (cdr (xile--msg-new_view '((file_path . "test_path/xile.scm")))) => "{\"id\":1,\"method\":\"new_view\",\"params\":{\"file_path\":\"test_path/xile.scm\"}}" )

(check (cdr (xile--msg-plugin-start "test_view" "syntect")) => "{\"id\":2,\"method\":\"plugin\",\"params\":{\"method\":\"start\",\"params\":{\"view_id\":\"test_view\",\"plugin_name\":\"syntect\"}}}")

(check (cdr (xile--msg-edit-insert "test_view" '())) => "{\"id\":3,\"method\":\"edit\",\"params\":{\"method\":\"insert\",\"view_id\":\"test_view\",\"params\":{}}}")
(check (cdr (xile--msg-edit-paste "test_view" '((chars . "password")))) => "{\"id\":4,\"method\":\"edit\",\"params\":{\"method\":\"paste\",\"view_id\":\"test_view\",\"params\":{\"chars\":\"password\"}}}")
(check (cdr (xile--msg-edit-move_right "test_view")) => "{\"id\":5,\"method\":\"edit\",\"params\":{\"method\":\"move_right\",\"view_id\":\"test_view\",\"params\":{}}}")

(check-report)
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
