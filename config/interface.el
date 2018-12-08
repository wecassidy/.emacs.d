;;; interface.el --- UI configuration

;;; Commentary:
;; Enables ido, loads the theme, load mode-line customization, etc.

;;; Code:
;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sanityinc-solarized-dark t)

;; Mode line
(require 'modeline)

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 4)))
(setq scroll-step 1) ; keyboard scroll one line at a time

;; Cursor
(blink-cursor-mode)

;; Don't ring an audible bell
(setq visible-bell 1)

;; Window size
(setq default-frame-alist '((fullscreen . maximized)))

;; Line/column numbers
(global-linum-mode t)
(column-number-mode t)

;; Highlight current line
(global-hl-line-mode)

;;; ido makes finding stuff better
(require 'ido)
(ido-mode 1)
(setq ido-case-fold t)

;; Deprioritize certain files (specifically, autogenerated LaTeX stuff)
(setq ido-file-extensions-order
      '(t ".log" ".aux" ".bbl" ".bcf" ".blg" ".out" ".run.xml"))

;; Flexible matching in ido
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)

;; Use a vertical list instead
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; ido everywhere (see also modes/wec-git.el, which enables ido in magit)
(ido-everywhere 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
;; smex: ido for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; Old M-x

;; Automatically sudo - based on http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name)
               (not (string-prefix-p "/ssh:" buffer-file-name))) ; Don't tramp when SSHing
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Auto-insert text
(add-hook 'find-file-hook 'auto-insert)

;; Use Symbola as a backup when characters aren't found
(set-fontset-font t nil "Symbola")

;; ibuffer: buffer list, but way more awesome
(require 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1))) ; Keep ibuffer up to date

(provide 'interface)
;;; interface.el ends here
