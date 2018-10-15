;;; code.el --- settings for programming

;;; Commentary:
;; Settings common to all languages.  yasnippet, indentation, linting,
;; etc.

;;; Code:
;; Indentation
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?\|)

;; Don't softwrap
(set-default 'truncate-lines t)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Lisp
(setq inferior-lisp-program "ccl")

;; Autocomplete
(ac-config-default)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode)
(setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt))

;; Parentheses
(require 'electric)
(electric-pair-mode)
(electric-indent-mode)
(require 'paren)
(show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren t)
(color-theme-sanityinc-solarized--with-colors
 'dark
 (set-face-attribute 'show-paren-match nil
                     :foreground blue
                     :inverse-video nil
                     :underline t)
 (set-face-attribute 'show-paren-mismatch nil
                     :foreground red
                     :background base03
                     :inverse-video t))

;; Code linting
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-status-emoji-mode))
(setq flycheck-emacs-lisp-load-path 'inherit)

;; Buffer-local modes that should always be on
(add-hook 'prog-mode-hook (lambda ()
                            (subword-mode) ; M-f/M-b through camelCase correctly
                            (flyspell-prog-mode))) ; Spellcheck for coding

;; Prettify symbols
(global-prettify-symbols-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (push '("!=" . ?≠) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)))

(provide 'code)
;;; code.el ends here
