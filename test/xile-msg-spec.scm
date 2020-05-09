#!/usr/bin/env -S guile --no-auto-compile -s
!#

(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (srfi srfi-64)
             (xile message))


(test-begin "message-serialization")

(test-equal "Init notification without parameters"
  (cdr (xile-msg-init))
  "{\"method\":\"client_started\",\"params\":{}}")
(test-equal "Init notification with a config directory"
  (cdr (xile-msg-init #:config_dir "test_path/togo"))
  "{\"method\":\"client_started\",\"params\":{\"config_dir\":\"test_path/togo\"}}" )

(test-equal "New View message"
  (cdr (xile-msg-new_view #:file_path "test_path/xile.scm"))
  "{\"id\":1,\"method\":\"new_view\",\"params\":{\"file_path\":\"test_path/xile.scm\"}}" )

(test-equal "Plugin->Start message"
  (cdr (xile-msg-plugin-start "test_view" "syntect"))
  "{\"id\":2,\"method\":\"plugin\",\"params\":{\"method\":\"start\",\"params\":{\"view_id\":\"test_view\",\"plugin_name\":\"syntect\"}}}")

(test-equal "Edit->Copy message"
  (cdr (xile-msg-edit-copy "test_view"))
  "{\"id\":3,\"method\":\"edit\",\"params\":{\"method\":\"copy\",\"view_id\":\"test_view\",\"params\":{}}}")
(test-equal "Edit->Insert message with empty data"
  (cdr (xile-msg-edit-insert "test_view" ""))
  "{\"id\":4,\"method\":\"edit\",\"params\":{\"method\":\"insert\",\"view_id\":\"test_view\",\"params\":{\"chars\":\"\"}}}")
(test-equal "Edit->Paste message with data"
  (cdr (xile-msg-edit-paste "test_view" "password"))
  "{\"id\":5,\"method\":\"edit\",\"params\":{\"method\":\"paste\",\"view_id\":\"test_view\",\"params\":{\"chars\":\"password\"}}}")
(test-equal "Edit->Move Right notification"
  (cdr (xile-msg-edit-move_right "test_view"))
  "{\"method\":\"edit\",\"params\":{\"method\":\"move_right\",\"view_id\":\"test_view\",\"params\":{}}}")

(test-end "message-serialization")

;; Make the exit code match the total number of failed tests for CI purposes.
(exit (+ (test-runner-fail-count (test-runner-get)) (test-runner-xpass-count (test-runner-get))))
