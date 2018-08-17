;;; interface.el --- UI configuration

;;; Commentary:
;; Enables ido, loads the theme, load mode-line customization, etc.

;;; Code:
;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sanityinc-solarized-dark t)

;; Mode line
(require 'modeline)

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 4)))
(setq scroll-step 1) ; keyboard scroll one line at a time

;; Cursor
(blink-cursor-mode)

;; Spellchecking
(require 'ispell)
(setq-default ispell-program-name "/usr/bin/aspell")
(setq ispell-dictionary "en_CA")

;; Don't ring an audible bell
(setq visible-bell 1)

;; Window size
(setq default-frame-alist '((fullscreen . maximized)))

;; Line/column numbers
(global-linum-mode t)
(column-number-mode t)

;; ido makes finding stuff better
(require 'ido)
(ido-mode t)
(setq ido-case-fold t)

; Automatically sudo - based on http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name)
               (not (string-prefix-p "/ssh:" buffer-file-name))) ; Don't tramp when SSHing
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; Terminal
(global-set-key (kbd "C-c C-t") 'ansi-term)

;; Smart C-a
(defun move-beginning-of-line-dwim ()
  "Move point to first non-whitespace character or the beginning of the line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (beginning-of-line-text)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'move-beginning-of-line-dwim)

;; Auto-insert text
(add-hook 'find-file-hook 'auto-insert)

;; Use Symbola as a backup when characters aren't found
(set-fontset-font t nil "Symbola")

;; Better regexes
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

(provide 'interface)
;;; interface.el ends here
