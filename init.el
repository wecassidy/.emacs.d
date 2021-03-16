;;; init.el --- Emacs global configuration. Mostly loads other files.

;;; Commentary:
;; Settings grouped around a theme (e. g. UI, packages, etc.) are in
;; ~/.emacs.d/config/*.el.  Settings for specific modes are in
;; ~/.emacs.d/config/modes/*.el.

;;; Code:
;; (package-initialize) ; placate package.el

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Update load-path
(add-to-list 'load-path (directory-file-name "~/.emacs.d/config/"))
(add-to-list 'load-path (directory-file-name "~/.emacs.d/config/modes/"))

;; Disable startup screen
(setq inhibit-startup-screen t)
(setq initial-buffer-choice "~") ; Dired ~/ on startup

;; Packages
(require 'wec-packages)

(require 'better-defaults)

;; UI (theme, fonts, cursor, etc.)
(require 'interface)

;; Editing
(require 'editing)

;; Programming
(require 'code)

(global-set-key (kbd "C-c C-q C-c") 'quick-calc)

;; Encoding - UTF-8
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; $PATH
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin" "/home/wec/.local/bin" "/home/wec/.platformio/penv/bin" "/home/wec/.cargo/bin")))
(setenv "PATH"
         (concat
          "/usr/texbin" ":" "/usr/local/bin" ":" "/home/wec/.local/bin" ":" "/home/wec/.platformio/penv/bin" ":"
          (getenv "PATH")))

;; Save backup and autosave files to temporary-file-directory, not the current directory
(setq temporary-file-directory "~/tmp/emacs")
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; PDFs
(use-package pdf-tools
   :pin manual
   :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-width)
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; Mode-specific customizations
(require 'wec-arduino)
(require 'wec-calc)
(require 'wec-cmodes)
(require 'wec-csv)
(require 'wec-dired)
(require 'wec-git)
(require 'wec-latex)
(require 'wec-lisp)
(require 'wec-org)
(require 'wec-py)
(require 'wec-r)
(require 'wec-rust)
(require 'wec-sh)
(require 'wec-web)

(put 'upcase-region 'disabled nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init.el)
;;; init.el ends here
(put 'c-electric-semi&comma 'disabled nil)
