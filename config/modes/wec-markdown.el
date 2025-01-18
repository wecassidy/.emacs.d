;;; wec-markdown.el --- a beautiful markdown editing interface  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'display-line-numbers-exempt-modes 'markdown-mode)
(add-hook 'markdown-mode-hook 'wec-setup-md-display)

(defun wec-setup-md-display ()
  "Set up fonts and such for a pleasant markdown experience."
  (visual-fill-column-mode)
  (variable-pitch-mode)
  (prettify-symbols-mode)

  (face-remap-add-relative 'default nil :height 140)
  (face-remap-add-relative 'fixed-pitch nil :height 140))

(provide 'wec-markdown)
;;; wec-markdown.el ends here
