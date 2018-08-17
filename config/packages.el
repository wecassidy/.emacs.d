;;; ~/.emacs.d/config/packages.el: package management settings

(require 'package)
(add-to-list 'package-archives '("gnus" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archive-priorities '("gnus . 1")) ; Prefer HTTPS GNU over other archives
(package-initialize)

(require 'use-package)
(setq auto-package-update-interval 7)
(auto-package-update-maybe) ; Update packages once per week
