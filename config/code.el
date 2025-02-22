;;; code.el --- settings for programming

;;; Commentary:
;; Settings common to all languages.  Indentation, linting, etc.

;;; Code:
;; Indentation
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq lua-indent-level 2)
(setq lua-indent-close-paren-align nil)

;; Don't softwrap
(set-default 'truncate-lines t)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'wec-auto-complete)

;;; Parentheses and other pairs
;; Auto-insert pairs
(use-package smartparens
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (setq sp-show-pair-from-inside t))

(electric-indent-mode)

;; Code linting
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;; Buffer-local modes that should always be on
(add-hook 'prog-mode-hook (lambda ()
                            (subword-mode) ; M-f/M-b through camelCase correctly
                            (flyspell-prog-mode))) ; Spellcheck for coding

(add-hook 'vhdl-mode-hook (lambda () (vhdl-electric-mode)))

;; `arduino-mode' doesn't inherit from `prog-mode', so buffer-local
;; modes have to be added on their own
(add-hook 'arduino-mode-hook (lambda ()
                            (subword-mode)
                            (flyspell-prog-mode)
                            (auto-complete-mode)))

(add-hook 'csv-mode-hook
          (lambda ()
            (csv-align-fields nil (point-min) (point-max))
            (csv-header-line)))

;; Keyboard shortcut to launch a terminal
(global-set-key (kbd "C-c C-t") 'ansi-term)
(setq sh-basic-offset 2)
(setq sh-indentation 2)

(add-hook 'rust-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq rust-format-on-save t)
            (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)))

;; R: remaps C-up and C-down to match input in history in inferior ESS
;; buffers. Remaps C-x t to complete a filename in inferior ESS
;; buffers.
(add-hook 'inferior-ess-mode-hook
          #'(lambda nil
             (define-key inferior-ess-mode-map [\C-up]
               'comint-previous-matching-input-from-input)
             (define-key inferior-ess-mode-map [\C-down]
               'comint-next-matching-input-from-input)
             (define-key inferior-ess-mode-map [\C-x \t]
               'comint-dynamic-complete-filename)))

(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status))
  :config
  (setq git-commit-major-mode 'org-mode)
  (setq magit-completing-read-function 'magit-ido-completing-read))

;; Display symbols (e. g. \alpha) as unicode
(setq org-pretty-entities t)

;; Bug tracking keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)")
        (sequence "TO-WRITE(w)" "NOTES(n)" "DRAFT(r)" "|" "DONE(d)")
        (sequence "BUG(b)" "WORKING(w)" "|" "FIXED(f)")))
(setq org-use-fast-todo-selection 'prefix) ; Use a prefix argument to select todo keywords by single-letter
(add-hook 'org-mode-hook 'org-indent-mode)
(org-babel-do-load-languages
      'org-babel-load-languages
      '((python . t)))
(setq org-startup-folded t)

;; Make latex previews bigger
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(add-hook 'rst-mode-hook #'visual-fill-column-mode)

(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

(require 'lsp-mode)
(add-to-list 'lsp-language-id-configuration '(asy-mode . "asymptote"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("asy" "-lsp"))
                  :activation-fn (lsp-activate-on "asymptote")
                  :major-modes '(asy-mode)
                  :server-id 'asyls))

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Treesitter
(add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter/linux/")
(add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter/windows/")
(setq major-mode-remap-alist
 '((bash-mode . bash-ts-mode)
   (c-mode . c-ts-mode)
   (c++-mode . c++-ts-mode)
   (cmake-mode . cmake-ts-mode)
   (css-mode . css-ts-mode)
   (java-mode . java-ts-mode)
   (js2-mode . js-ts-mode)
   (json-mode . json-ts-mode)
   (python-mode . python-ts-mode)
   (conf-toml-mode . toml-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (yaml-mode . yaml-ts-mode)))
(setq treesit-font-lock-level 4) ; Highlight everything

(provide 'code)
;;; code.el ends here
