;;; elisp-eval-tests.el --- Tests for elisp-eval -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'elisp-eval)

(defvar elisp-eval-test-value nil)

(ert-deftest elisp-eval-string-evaluates-all-forms-in-source-order ()
  (should (equal (elisp-eval-string "(+ 1 2)\n(* 3 4)")
                 '(3 12))))

(ert-deftest elisp-eval-string-uses-lexical-binding ()
  (should (equal (elisp-eval-string
                  "(let ((value 42)) (funcall (lambda () value)))")
                 '(42))))

(ert-deftest elisp-eval-string-accepts-whitespace-and-comments ()
  (should (equal (elisp-eval-string
                  "; before\n(+ 1 2) ; after\n\n")
                 '(3)))
  (should-not (elisp-eval-string "  ; only a comment\n")))

(ert-deftest elisp-eval-string-signals-incomplete-input ()
  (should-error (elisp-eval-string "(+ 1") :type 'end-of-file))

(ert-deftest elisp-eval-string-does-not-swallow-evaluation-end-of-file ()
  (should-error
   (elisp-eval-string "(signal 'end-of-file '(from-evaluated-code))")
   :type 'end-of-file))

(ert-deftest elisp-eval-buffer-evaluates-in-target-buffer ()
  (let ((source-buffer (generate-new-buffer " *elisp-eval-source*"))
        (target-buffer (generate-new-buffer " *elisp-eval-target*")))
    (unwind-protect
        (progn
          (with-current-buffer source-buffer
            (emacs-lisp-mode)
            (insert "(setq-local elisp-eval-test-value 17)\n"
                    "(current-buffer)")
            (goto-char 2))
          (should (equal (elisp-eval--eval-buffer source-buffer target-buffer)
                         (list 17 target-buffer)))
          (with-current-buffer target-buffer
            (should (= elisp-eval-test-value 17)))
          (with-current-buffer source-buffer
            (should (= (point) 2))))
      (kill-buffer source-buffer)
      (kill-buffer target-buffer))))

(ert-deftest elisp-eval-command-has-no-implicit-prefix-argument ()
  (should (equal (interactive-form #'elisp-eval--eval)
                 '(interactive nil))))

(provide 'elisp-eval-tests)
;;; elisp-eval-tests.el ends here
