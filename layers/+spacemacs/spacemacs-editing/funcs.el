;;; funcs.el --- Spacemacs editing Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; uuidgen
;; TODO spacemacs/uuidgen-3 and spacemacs/uuidgen-5

(defun spacemacs/uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun spacemacs/uuidgen-4 (arg)
  "Return an UUID from random numbers (UUIDv4).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-4)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun spacemacs/insert-file-name ()
  "Insert path interactively"
  (interactive)
  (let ((path (read-file-name "Insert: ")))
    (insert path)))

(defun spacemacs/insert-directory-name ()
  "Insert path interactively"
  (interactive)
  (let ((path (read-directory-name "Insert: ")))
    (insert path)))

(defun spacemacs/insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (let ((time-strs (mapcar (lambda (format)
                             (format-time-string format))
                           '("%c"
                             "%Y-%m-%d %T"
                             "%Y-%m-%d %T%z"
                             "%Y-%m-%d"
                             "%FT%T%z"))))
    (insert (completing-read "Insert: " time-strs))))

(defvar spacemacs-smart-tab-keymap
 (let ((map (make-sparse-keymap)))
   (define-key map [(tab)]       'spacemacs/smart-tab)
   (define-key map (kbd "TAB")   'spacemacs/smart-tab)
   map))

(define-minor-mode spacemacs-smart-tab-mode
    "minor-mode which do smart work for tab key."
    :init-value nil
    :global     nil
    :lighter    ""
    :keymap spacemacs-smart-tab-keymap)

(defun spacemacs/smart-tab ()
  "Extension for indent-for-tab-command.

No changes/indentation are made after calling
indent-for-tab-command, then indent the sexp or remove empty
lines where is neccessary.
"
  (interactive)
  (let* ((old-tick (buffer-chars-modified-tick))
         (old-point (point))
         (tab-command
          (let (spacemacs-smart-tab-mode)
            (key-binding (kbd "TAB") t))))
    (when tab-command
         (call-interactively tab-command))
    (when (and (eq old-point (point))
               (eq old-tick (buffer-chars-modified-tick)))
      (cond
       ((looking-at "\\s\(")
        (save-excursion
          (indent-region (point)
                         (save-excursion (forward-sexp) (point)))))
       ((looking-back "[ \t]+$")
        (delete-horizontal-space)
        (delete-blank-lines)
        (forward-line 1)
        (back-to-indentation))))))
