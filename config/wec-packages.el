;;; wec-packages.el --- package management settings
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("gnus" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archive-priorities '("gnus . 1")) ; Prefer HTTPS GNU over other archives
(package-initialize)

;; Automatically update packages on startup
(require 'use-package)
(setq auto-package-update-interval 7)
(auto-package-update-maybe) ; Update packages once per week

;; Use a better package menu
(require 'paradox)
(paradox-enable)
(setq paradox-execute-asynchronously t) ; Update, install, etc in the background

(provide 'wec-packages)
;;; wec-packages.el ends here
