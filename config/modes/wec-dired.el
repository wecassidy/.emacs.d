;;; wec-dired.el --- settings for dired
;;; Commentary:
;; Thus far, only sets the ls switches
;;; Code:
(require 'dired)
(setq dired-listing-switches "-lAhF --group-directories-first")

;; Automatically hide boring files
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

(dir-locals-set-class-variables ; Files to hide in ~/
 'dired-home
 `((dired-mode . ((dired-omit-mode . t)
                  (dired-omit-files . ,(rx line-start
                                           (or ".adobe"
                                               (and ".aspell" (one-or-more anything))
                                               (and ".bash" (one-or-more anything))
                                               ".cache"
                                               ".cups"
                                               (and ".dropbox" (zero-or-more anything))
                                               ".electron"
                                               ".esd_auth"
                                               ".fonts"
                                               ".gnupg"
                                               ".gphoto"
                                               ".hplip"
                                               ".ICEauthority"
                                               ".ipython"
                                               ".lesshst"
                                               ".local"
                                               ".macromedia"
                                               ".mozilla"
                                               ".node-gyp"
                                               ".npm"
                                               ".pki"
                                               ".pylint.d"
                                               ".python_history"
                                               ".wget-hsts"
                                               ".xournal"
                                               (and ".zcompdump" (zero-or-more anything))
                                               ".zenmap")
                                           line-end))))))

(dir-locals-set-directory-class "~/" 'dired-home)

(provide 'wec-dired)
;;; wec-dired.el ends here
