;;; a51-toc.el --- generate a navigable table of contents for A51 assembley  -*- lexical-binding: t; -*-
;; Copyright (C) 2021  Wesley Cassidy
;; Author: Wesley Cassidy <wec@wec-manjaro>
;;; Commentary:
;;; Code:

(defvar-local a51-toc-source-buffer nil
  "Buffer that called a51-toc")

(defvar a51-toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'a51-toc-visit-label)
    map)
  "Keymap used in the A51 TOC buffer")

;;;###autoload
(define-derived-mode a51-toc-mode special-mode "TOC"
  "Major mode for managing table of contents (labels) for A51
files.

Here are all the local bindings.

\\{a51-toc-mode-map}")

(defun a51-toc-find-labels ()
  "Generate a list of labels in the current buffer."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (cl-loop while (re-search-forward a51-label-regexp nil t)
               collect (cons (match-string-no-properties 1) (match-beginning 0))))))

;;;###autoload
(defun a51-toc ()
  "Pop up a buffer containing the table of contents for the current file."
  (interactive)
  (let ((label-list (a51-toc-find-labels))
        (source-buffer (current-buffer)))
    (pop-to-buffer (format "*%s TOC*" (buffer-name)))
    (setq-local a51-toc-source-buffer source-buffer)
    (dolist (label label-list)
      (insert (car label))
      (add-text-properties
       (point-at-bol) (point-at-eol)
       (list 'a51-label-source (cdr label)))
      (newline))
    (a51-toc-mode)))

(defun a51-toc-visit-label ()
  "Go to the label corresponding to the current line in the toc."
  (interactive)
  (let ((dest-point (get-text-property (point) 'a51-label-source)))
    (pop-to-buffer a51-toc-source-buffer)
    (goto-char dest-point)))

(provide 'a51-toc)
;;; a51-toc.el ends here
