;;; interface.el --- UI configuration

;;; Commentary:
;; Enables ido, loads the theme, load mode-line customization, etc.

;;; Code:
;; Theme

;; Basic fonts

(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono")
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono"))
(when (member "Noto Sans" (font-family-list))
  (set-face-attribute 'variable-pitch nil :font "Noto Sans"))

(set-face-attribute 'default nil :height 120)

;; Use Symbola as a backup when characters aren't found
(set-fontset-font t nil "Symbola")

;; Scrolling
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 4)))
(use-package good-scroll
  :config
  (good-scroll-mode 1))
(setq scroll-step 1) ; keyboard scroll one line at a time

;; Window size
(setq default-frame-alist '((fullscreen . maximized)))
(setq-default window-combination-resize t)

;; Don't show the splash screen when launching with a file
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)

;; Line/column numbers
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '()
  "Major modes on which to disable the linum mode, exempts them
from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers but excempting certain major modes
defined in `display-line-numbers-exempt-modes'"
  (unless (or (member major-mode display-line-numbers-exempt-modes) (minibufferp))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)
(column-number-mode)

;;; ido makes finding stuff better
(ido-mode 1)
(setq ido-case-fold t)

;; Flexible matching in ido
(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)) ; disable ido faces to see flx highlights.

;; Use a vertical list instead
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; ido everywhere (see also modes/wec-git.el, which enables ido in magit)
(ido-everywhere 1)
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package amx ; ido for M-x
  :config
  (amx-mode 1))

;; Lifted from https://github.com/bbatsov/crux
;; modified from https://www.emacswiki.org/emacs/TransposeWindows
;;;###autoload
(defun crux-transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)
(windmove-default-keybindings) ; Use shift-arrows to switch between windows

;; Automatically sudo - based on http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") #'er-sudo-edit)

;; ibuffer: buffer list, but way more awesome
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  #'(lambda ()
	     (ibuffer-auto-mode 1))) ; Keep ibuffer up to date

(add-hook 'Info-selection-hook
          (lambda ()
            (variable-pitch-mode)
            (info-colors-fontify-node)
            (display-line-numbers-mode nil)))

(use-package which-key
  :config
  (which-key-mode))

(setq dired-listing-switches "-lAhF --group-directories-first")

(provide 'interface)
;;; interface.el ends here
