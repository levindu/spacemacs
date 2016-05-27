;;; funcs.el --- Better Emacs Defaults Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun spacemacs/smart-move-end-of-line (arg)
  "Move point back to non-whitespace of end of line.

Move point to the last non-whitespace character on this line.
If point is already there, move to the end of the line.
Effectively toggle between the first non-whitespace character and
the end of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (move-end-of-line (1- arg))))
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (skip-syntax-backward " " (line-beginning-position))
    (when (= orig-point (point))
      (move-end-of-line 1))))


(defun spacemacs/flush-lines ()
  "Force flush lines from beginning of buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'flush-lines))))

(defun spacemacs/keep-lines ()
  "Force keep lines from beginning of buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-buffer)
      (call-interactively 'keep-lines))))

(defun spacemacs/next-same-mode-buffer ()
  (interactive)
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (next-buffer)
      (setq next-mode major-mode))))

(defun spacemacs/backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and
`backward-kill-word' otherwise. ARG is passed to
`backward-kill-word' if no region is active."
  (interactive "p")
  (if (region-active-p)
      ;; call interactively so kill-region handles rectangular selection
      ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))
