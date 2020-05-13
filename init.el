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

;; Encoding - UTF-8
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; $PATH
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin")))
(setenv "PATH"
         (concat
          "/usr/texbin" ":" "/usr/local/bin" ":"
          (getenv "PATH")))

;; Save backup and autosave files to temporary-file-directory, not the current directory
(setq temporary-file-directory "~/tmp/emacs")
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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
(require 'wec-platformio)
(require 'wec-py)
(require 'wec-r)
(require 'wec-sh)
(require 'wec-text)
(require 'wec-web)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(package-selected-packages
   (quote
    (lsp-ui dap-mode company-lsp lsp-java doom-modeline nlinum powershell smartparens esup sr-speedbar company-quickhelp company company-arduino company-auctex company-jedi company-math company-reftex company-statistics company-web mines matlab-mode paradox shift-number ido-describe-bindings ido-vertical-mode flx-ido ido-completing-read+ smex better-defaults flycheck-pycheckers csharp-mode 2048-game yasnippet-snippets visual-regexp-steroids jedi-direx flycheck-status-emoji yaml-mode jedi python-docstring gulp-task-runner use-package diminish rainbow-mode pymacs arduino-mode gitignore-mode ebib web-mode-edit-element slime scss-mode markdown-mode magit latex-extra json-mode jinja2-mode java-snippets java-imports ess emmet-mode csv-mode common-lisp-snippets color-theme-sanityinc-solarized cdlatex auto-package-update apache-mode)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((dired-omit-files . "^\\(?:\\.adobe\\|\\.aspell\\(?:.\\|
\\)+\\|\\.bash\\(?:.\\|
\\)+\\|\\.cache\\|\\.cups\\|\\.dropbox\\(?:.\\|
\\)*\\|\\.electron\\|\\.esd_auth\\|\\.fonts\\|\\.gnupg\\|\\.gphoto\\|\\.hplip\\|\\.ICEauthority\\|\\.ipython\\|\\.lesshst\\|\\.local\\|\\.macromedia\\|\\.mozilla\\|\\.node-gyp\\|\\.npm\\|\\.pki\\|\\.pylint\\.d\\|\\.python_history\\|\\.wget-hsts\\|\\.xournal\\|\\.zcompdump\\(?:.\\|
\\)*\\|\\.zenmap\\)$"))))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "#839496" :background "#002b36" :underline t))))
 '(company-scrollbar-bg ((t (:background "#657b83"))))
 '(company-scrollbar-fg ((t (:background "#93a1a1"))))
 '(company-tooltip ((t (:inherit default :background "#073642"))))
 '(company-tooltip-common ((t (:foreground "#268bd2"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

(put 'upcase-region 'disabled nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init.el)
;;; init.el ends here
