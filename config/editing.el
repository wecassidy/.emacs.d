;;; editing.el --- customizations that affect editing in all modes  -*- lexical-binding: t; -*-
;;; Commentary:
;; Code in this file should not be mode-specific.  That kind of code
;; goes in modes/<mode>.el.

;;; Code:
(delete-selection-mode)
(setq auto-save-default t)

;; Spellchecking
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

;; Better regexes
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

(defun wec-return-in-pair (point)
  "On return in a balanced pair, place point inside the pair, ready to edit.

If | is point:
  ... {|}
becomes
  ... {
    |
   }"
  (interactive "*d")

  (let ((syntax-class (car (syntax-after (- point 1))))
        (pair-character (cdr (syntax-after (- point 1)))))
    (if (and (equal syntax-class 4) ;; Open paren
             (equal (char-after point) pair-character))
        (progn
          (newline-and-indent)
          (forward-line -1)
          (end-of-line)
          (newline-and-indent))
      (newline-and-indent))))
(global-set-key (kbd "RET") 'wec-return-in-pair)

;; undo-tree
(global-undo-tree-mode)

;; Line wrapping
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package visual-fill-column-mode
  :init
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-fringes-outside-margins t)
  :config
  (visual-line-mode)
  :hook (LaTeX-mode))

(use-package visual-line-mode
  :init
  (add-hook 'visual-line-mode-hook 'turn-off-auto-fill)
  :hook visual-fill-column-mode)

(provide 'editing)
;;; editing.el ends here
