;;; wec-packages.el --- package management settings
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("gnus" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archive-priorities '("gnus . 1")) ; Prefer HTTPS GNU over other archives
(package-initialize)

(require 'use-package)

;; Use a better package menu
(use-package paradox
  :defer 2
  :init
  (setq paradox-execute-asynchronously t)
  :config
  (paradox-enable))

;; Automatically update packages on startup
(setq auto-package-update-interval 7)
(auto-package-update-maybe) ; Update packages once per week

(provide 'wec-packages)
;;; wec-packages.el ends here
