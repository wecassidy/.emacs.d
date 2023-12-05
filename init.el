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

;; Packages
(require 'wec-packages)
(use-package better-defaults)

(require 'interface)
(require 'editing)
(require 'code)

;; Encoding - UTF-8
(prefer-coding-system 'utf-8)

;; $PATH
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/texbin" "/home/wec/.local/bin" "/home/wec/.platformio/penv/bin" "/home/wec/.cargo/bin")))

;; Mode-specific customizations
(require 'wec-cmodes)
(require 'wec-latex)
(require 'wec-py)
(require 'wec-web)

(put 'upcase-region 'disabled nil)
(put 'c-electric-semi&comma 'disabled nil)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init.el)
;;; init.el ends here
