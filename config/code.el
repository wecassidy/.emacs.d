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
(setq highlight-indent-guides-responsive 'top)

;; Don't softwrap
(set-default 'truncate-lines t)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'wec-auto-complete)

;; Yasnippet
(use-package yasnippet
  :defer 1
  :init
  (setq yas-prompt-functions '(yas-ido-prompt yas-x-prompt))
  :config
  (yas-global-mode))

;;; Parentheses and other pairs
;; Auto-insert pairs
(use-package smartparens-config
  :ensure smartparens
  :config (show-smartparens-global-mode t)
  (use-package smartparens-mode
    :hook (prog-mode text-mode)))

(electric-indent-mode)

;; Highlight matching parentheses
(require 'paren)
(show-paren-mode)
(setq show-paren-delay 0)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren t)

;; Also highlight quotes - from https://emacs.stackexchange.com/a/43687/14703
(defun show-paren--match-quotes ()
  "Find the opening and closing quote marks of a string for
`show-paren-mode'."
  (let ((ppss (syntax-ppss)))
    ;; In order to distinguish which quote is opening and which is starting,
    ;; check that that point is not within a string (or comment, for that
    ;; matter).  Also ignore escaped quotes.
    (unless (or (nth 8 ppss) (nth 5 ppss))
      (or
       (and (not (bobp))
            (eq 7 (car-safe (syntax-after (1- (point)))))
            (save-excursion
              (let ((end (point))
                    (ppss (syntax-ppss (1- (point)))))
                (when (nth 3 ppss)
                  (let ((beg (nth 8 ppss)))
                    (list beg
                          (1+ beg)
                          (1- end)
                          end))))))
       (and (not (eobp))
            (eq 7 (car-safe (syntax-after (point))))
            (save-excursion
              (let ((beg (point)))
                (condition-case nil
                    (progn
                      (forward-sexp 1)
                      (list beg
                            (1+ beg)
                            (1- (point))
                            (point)))))))))))

(advice-add 'show-paren--default :after-until #'show-paren--match-quotes)

;; Parenthesis matching face
(require 'color-theme-sanityinc-solarized)
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

(provide 'code)
;;; code.el ends here
