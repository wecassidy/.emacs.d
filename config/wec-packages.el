;;; wec-packages.el --- package management settings
;;; Commentary:
;;; Code:
(add-to-list 'package-archives '("gnus" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archive-priorities '("gnus . 1")) ; Prefer HTTPS GNU over other archives
(package-initialize)

;; Use a better package menu
(use-package paradox
  :defer 2
  :init
  (setq paradox-execute-asynchronously t)
  :config
  (paradox-enable))

;; Automatically update packages on startup
(setq auto-package-update-interval 1)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-maybe)

(provide 'wec-packages)
;;; wec-packages.el ends here
