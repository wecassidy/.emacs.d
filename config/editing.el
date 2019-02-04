;;; editing.el --- customizations that affect editing in all modes  -*- lexical-binding: t; -*-
;;; Commentary:
;; Code in this file should not be mode-specific.  That kind of code
;; goes in modes/<mode>.el.

;;; Code:
(delete-selection-mode)

;; Spellchecking
(require 'ispell)
(setq-default ispell-program-name "/usr/bin/aspell")
(setq ispell-dictionary "en_CA")

;; Smart C-a
(defun move-beginning-of-line-dwim ()
  "Move point to first non-whitespace character or the beginning of the line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (beginning-of-line-text)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'move-beginning-of-line-dwim)

;; Increment and decrement numbers
(require 'shift-number)
(global-set-key (kbd "C-c C-+") 'shift-number-up)
(global-set-key (kbd "C-c C--") 'shift-number-down)

;; Insert the date at point
(defun wec-insert-date ()
  "Insert the current date in ISO 8601 (i.e. yyyy-mm-dd) format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(global-set-key (kbd "C-c d") 'wec-insert-date)

;; Better regexes
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

(provide 'editing)
;;; editing.el ends here
