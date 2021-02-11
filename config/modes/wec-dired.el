;;; wec-dired.el --- settings for dired
;;; Commentary:
;; Thus far, only sets the ls switches
;;; Code:
(require 'dired)
(setq dired-listing-switches "-lAhF --group-directories-first")

(provide 'wec-dired)
;;; wec-dired.el ends here
