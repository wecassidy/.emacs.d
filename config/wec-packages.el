;;; wec-packages.el --- package management settings
;;; Commentary:
;;; Code:
(add-to-list 'package-archives '("gnus" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archive-priorities '("gnus . 1")) ; Prefer HTTPS GNU over other archives
(package-initialize)

;; Use a better package menu
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Automatically update packages on startup
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide 'wec-packages)
;;; wec-packages.el ends here
