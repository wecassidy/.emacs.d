;;; wec-git.el --- settings for git and magit

;;; Commentary:
;; See also ~/.gitconfig and ~/.gitignore_global

;;; Code:
(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status))
  :config
  (setq git-commit-major-mode 'org-mode)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (color-theme-sanityinc-solarized--with-colors
   'dark
   (set-face-attribute 'magit-diff-file-heading nil
                       :weight 'normal
                       :foreground violet)))

(provide 'wec-git)
;;; wec-git.el ends here
