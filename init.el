;;; init.el --- Emacs global configuration. Mostly loads other files.

;;; Commentary:
;; Settings grouped around a theme (e. g. UI, packages, etc.) are in
;; ~/.emacs.d/config/*.el.  Settings for specific modes are in
;; ~/.emacs.d/config/modes/*.el.

;;; Code:
;; (package-initialize) ; placate package.el

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

;; doc-view-mode
(require 'doc-view)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq doc-view-continuous t)

;; Mode-specific customizations
(require 'wec-arduino)
(require 'wec-calc)
(require 'wec-cpp)
(require 'wec-csharp)
(require 'wec-dired)
(require 'wec-git)
(require 'wec-java)
(require 'wec-json)
(require 'wec-lisp)
(require 'wec-org)
(require 'wec-py)
(require 'wec-r)
(require 'wec-sh)
(require 'wec-tex)
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
    (sr-speedbar company-quickhelp company company-arduino company-auctex company-jedi company-math company-reftex company-statistics company-web mines matlab-mode paradox shift-number ido-describe-bindings ido-vertical-mode flx-ido ido-completing-read+ smex better-defaults flycheck-pycheckers csharp-mode 2048-game yasnippet-snippets visual-regexp-steroids jedi-direx flycheck-status-emoji yaml-mode jedi python-docstring telephone-line gulp-task-runner use-package diminish rainbow-mode pymacs arduino-mode gitignore-mode ebib web-mode-edit-element slime scss-mode markdown-mode magit latex-extra json-mode jinja2-mode jdee java-snippets java-imports ess emmet-mode csv-mode common-lisp-snippets color-theme-sanityinc-solarized cdlatex auto-package-update apache-mode)))
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
 '(company-scrollbar-bg ((t (:background "#000053c56933"))))
 '(company-scrollbar-fg ((t (:background "#00003f624f99"))))
 '(company-tooltip ((t (:inherit default :background "#00003327403d"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

(put 'upcase-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
