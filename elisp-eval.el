;;; elisp-eval.el --- Configure eval -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elisp-eval
;; Keywords: lisp
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Eval expression in plain buffer instead of minibuffer.

;; Commands

;; M-x `elisp-eval' (&optional inital-content)
;;      Eval expression in *eval-elisp* buffer instead of minibuffer.
;;
;;      This command makes the current buffer the target buffer of
;;      the elisp-eval.
;;
;;      It displays a buffer named "*eval-elisp*"
;;      in another window.
;;
;;      With optional INITAL-CONTENT insert it.

;; in "*eval-elisp*" buffer such commands are available:

;; C-<return>  `elisp-eval--eval'                - eval and stay in window
;; C-c C-c     `elisp-eval--eval-and-quit'       - eval and exit window
;; C-x 0       `elisp-eval-quit'                 - bury buffer
;; C-x s       `elisp-eval-save-history'         - save history
;; C-x C-s     `elisp-eval-save-current-element' - save current element
;; M-n         `elisp-eval-next-history-element' - insert next history element
;; M-p         `elisp-eval-prev-history-element' - insert previous history element

;; global commands

;; M-x `elisp-eval-region-or-last-sexp'
;;      Eval active region or sexp at point.

;; M-x `elisp-eval-cleanup-history'
;;      Cleanup history.

;; Customization

;; `elisp-eval-history-max-size'
;;      Max size for history.

;; `elisp-eval-history-file'
;;      File to save history.

;;; Code:

(require 'pp)
(defvar elisp-eval--buffer-name "*eval-elisp*")
(defvar elisp-eval-target-buffer nil)
(defvar elisp-eval-target-window nil)
(defvar elisp-eval-window-config nil)

(defcustom elisp-eval-history-file (expand-file-name
                                    "elisp-eval-history.history"
                                    user-emacs-directory)
  "File to save history."
  :type 'file
  :group 'elisp-eval)

(defcustom elisp-eval-history-max-size 100
  "Max size for history."
  :type 'integer
  :group 'elisp-eval)

(defun elisp-eval-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `elisp-eval-unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert
       (let (print-level print-length)
         (prin1-to-string data))))))

(defun elisp-eval-unserialize (filename)
  "Read data serialized by `elisp-eval-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string))))))

(defvar elisp-eval-history nil)
(defvar elisp-eval-history-idx 0)
(defun elisp-eval-ensure-history-size ()
  "Check history size."
  (setq elisp-eval-history
        (if (> (length elisp-eval-history) elisp-eval-history-max-size)
            (seq-take elisp-eval-history elisp-eval-history-max-size)
          elisp-eval-history)))

;;;###autoload
(defun elisp-eval-string (str)
  "Read and evaluate all forms in STR.
Return the results of all forms as a list."
  (let ((next 0)
        ret)
    (condition-case nil
        (while t
          (setq ret (cons (funcall (lambda (ret)
                                     (setq next (cdr ret))
                                     (eval (car ret) t))
                                   (read-from-string str next))
                          ret)))
      (end-of-file))
    (nreverse ret)))

(defun elisp-eval-save-current-element ()
  "Save current content of \"*eval-elisp*\" buffer to `elisp-eval-history-file'."
  (interactive)
  (let ((elem (string-trim (buffer-substring-no-properties (point-min)
                                                           (point-max))))
        (history (elisp-eval-unserialize elisp-eval-history-file)))
    (if (string-empty-p elem)
        (message "Nothing to save")
      (setq history (delete elem history))
      (setq history (append history (list elem)))
      (elisp-eval-serialize history elisp-eval-history-file)
      (message "Saved"))))

(defun elisp-eval-save-history ()
  "Save history."
  (interactive)
  (when elisp-eval-history-file
    (elisp-eval-serialize (if (> (length elisp-eval-history)
                                 elisp-eval-history-max-size)
                              (seq-take elisp-eval-history
                                        elisp-eval-history-max-size)
                            elisp-eval-history)
                          elisp-eval-history-file)))

(defun elisp-eval-cleanup-history ()
  "Cleanup history."
  (interactive)
  (setq elisp-eval-history
        nil)
  (elisp-eval-save-history))

(defun elisp-eval-quit ()
  "Quit the eval buffer."
  (interactive)
  (bury-buffer elisp-eval--buffer-name)
  (set-window-configuration elisp-eval-window-config))

(defun elisp-eval-backward-sexp (&optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((pos (point)))
    (when-let ((end (ignore-errors
                      (backward-sexp (or n 1))
                      (point))))
      (unless (= end pos)
        end))))

(defun elisp-eval-count-sexps-in-buffer ()
  "Return amount of sexps in buffer ignoring comments."
  (save-excursion
    (goto-char (point-max))
    (let ((count 0))
      (while (elisp-eval-backward-sexp)
        (unless (or (nth 4 (syntax-ppss (point)))
                    (looking-at ";"))
          (setq count (1+ count))))
      count)))

(defun elisp-eval--eval (&optional quit)
  "Eval content of buffer `elisp-eval--buffer-name'.
Without prefix argument QUIT stay in buffer, otherwise exit."
  (interactive "p")
  (let ((count (with-current-buffer elisp-eval--buffer-name
                 (elisp-eval-count-sexps-in-buffer)))
        (str (buffer-substring-no-properties (point-min)
                                             (point-max)))
        (res))
    (setq elisp-eval-history (delete str elisp-eval-history))
    (setq elisp-eval-history (add-to-list 'elisp-eval-history str t))
    (elisp-eval-quit)
    (let ((print-length nil)
          (print-level nil)
          (print-circle nil)
          (result))
      (setq result (car
                    (with-current-buffer
                        elisp-eval-target-buffer
                      (elisp-eval-string
                       (if (> count 1)
                           (format "(progn %s)"
                                   str)
                         str)))))
      (setq res (or
                 (ignore-errors (pp-to-string
                                 result))
                 (prin1-to-string result)))
      (if (> (length res) 100)
          (with-output-to-temp-buffer "*elisp-eval-output*"
            (princ res standard-output)
            (with-current-buffer standard-output
              (let ((emacs-lisp-mode-hook nil))
                (emacs-lisp-mode)
                (when (fboundp 'visual-fill-column-mode)
                  (visual-fill-column-mode))
                (setq buffer-read-only nil)
                (set (make-local-variable 'font-lock-verbose) nil))))
        (princ res))
      (unless quit
        (with-current-buffer elisp-eval-target-buffer
          (elisp-eval))))))

(defun elisp-eval--eval-and-quit ()
  "Eval content of buffer `elisp-eval--buffer-name'."
  (interactive)
  (elisp-eval--eval t))

(defun elisp-eval-get-next-or-prev-history (n)
  "Return the N element of `elisp-eval--buffer-name'."
  (let* ((values elisp-eval-history)
         (max (1- (length values)))
         (sum (+ n elisp-eval-history-idx))
         (next-idx (if (and (>= sum 0)
                            (<= sum max))
                       sum
                     (if (> n 0) 0 (abs max)))))
    (setq elisp-eval-history-idx next-idx)
    (nth elisp-eval-history-idx elisp-eval-history)))

;;;###autoload
(defun elisp-eval-prev-history-element ()
  "Insert previous history content."
  (interactive)
  (erase-buffer)
  (when-let ((str (elisp-eval-get-next-or-prev-history -1)))
    (goto-char (point-min))
    (save-excursion (insert str))))

;;;###autoload
(defun elisp-eval-next-history-element ()
  "Insert next history content."
  (interactive)
  (erase-buffer)
  (when-let ((str (elisp-eval-get-next-or-prev-history 1)))
    (goto-char (point-min))
    (save-excursion (insert str))))

;;;###autoload
(defun elisp-eval-region-or-last-sexp ()
  "Eval active region or sexp at point."
  (interactive)
  (if-let ((reg (when
                    (and (region-active-p)
                         (use-region-p))
                  (string-trim (buffer-substring-no-properties
                                (region-beginning) (region-end))))))
      (elisp-eval-string reg)
    (when-let ((exp (pp-last-sexp)))
      (let ((type (car exp)))
        (cond ((memq type '(defvar defcustom defconst))
               (save-excursion
                 (save-restriction
                   (let ((end (point))
                         (beg))
                     (forward-sexp -1)
                     (setq beg (point))
                     (narrow-to-region beg end)
                     (eval-defun nil)))))
              (t (funcall-interactively #'pp-eval-last-sexp
                                        nil)))))))

(defvar elisp-eval-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") #'elisp-eval-quit)
    (define-key map (kbd "<C-return>") #'elisp-eval--eval)
    (define-key map (kbd "M-p") #'elisp-eval-prev-history-element)
    (define-key map (kbd "M-n") #'elisp-eval-next-history-element)
    (define-key map (kbd "C-c C-c") #'elisp-eval--eval-and-quit)
    (define-key map (kbd "C-x s") #'elisp-eval-save-history)
    (define-key map (kbd "C-x C-s") #'elisp-eval-save-current-element)
    map)
  "Elisp eval mode map.")

;;;###autoload
(defun elisp-eval (&optional inital-content)
  "Eval expression in *eval-elisp* buffer instead of minibuffer.

This command makes the current buffer the target buffer of
the `elisp-eval'.

It displays a buffer named \"*eval-elisp*\"
in another window.

With optional INITAL-CONTENT insert it."
  (interactive)
  (unless (and (string= (buffer-name) elisp-eval--buffer-name))
    (setq elisp-eval-target-buffer (current-buffer)
          elisp-eval-target-window (selected-window))
    (select-window (or (get-buffer-window elisp-eval--buffer-name)
                       (progn
                         (setq elisp-eval-window-config
                               (current-window-configuration))
                         (split-window (selected-window)
                                       (- (window-height) 10)))))
    (switch-to-buffer (get-buffer-create elisp-eval--buffer-name))
    (when (and elisp-eval-history-file
               (null elisp-eval-history))
      (setq elisp-eval-history (elisp-eval-unserialize elisp-eval-history-file)))
    (with-current-buffer elisp-eval--buffer-name
      (unless (memq last-command '(elisp-eval--eval-and-quit))
        (erase-buffer))
      (setq elisp-eval-history-idx 0)
      (font-lock-mode 1)
      (emacs-lisp-mode)
      (when (and (bound-and-true-p flycheck-mode)
                 (fboundp 'flycheck-mode))
        (flycheck-mode -1))
      (when (bound-and-true-p flymake-mode)
        (flymake-mode -1))
      (elisp-eval-mode)
      (when inital-content
        (insert inital-content)))))

;;;###autoload
(define-minor-mode elisp-eval-mode
  "Runs prettier on file save when this mode is turned on."
  :lighter " elisp-eval"
  :keymap elisp-eval-mode-map)

(provide 'elisp-eval)
;;; elisp-eval.el ends here
