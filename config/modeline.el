;;; modeline.el --- Emacs mode line

;;; Commentary:
;; Sets up telephone-line, defines solarized colour constants

;;; Code:
(require 'telephone-line)

;; Faces
(require 'color-theme-sanityinc-solarized)
(color-theme-sanityinc-solarized--with-colors
 'dark
 (defface buffer-info-face
   `((t (:foreground ,normal :background ,alt-background))) "")
 (defface buffer-name-face
   `((t (:foreground ,background :background ,magenta))) "")
 (defface major-mode-face
   `((t (:foreground ,background :background ,blue))) "")
 (defface minor-mode-face
   `((t (:foreground ,background :background ,violet))) "")
 (defface vc-face
   `((t (:foreground ,background :background ,green))) "")
 (defface battery-face
   `((t (:foreground ,background :background ,green))) "")
 (defface linum-face
   `((t (:foreground ,background :background ,blue))) ""))

(setq telephone-line-faces
      '((buffer-info . (buffer-info-face . buffer-info-face))
        (buffer-name . (buffer-name-face . buffer-name-face))
        (major . (major-mode-face . major-mode-face))
        (minor . (minor-mode-face . minor-mode-face))
        (vc . (vc-face . vc-face))
        (battery . (battery-face . battery-face))
        (linum . (linum-face . linum-face))
        (evil . telephone-line-evil-face)
        (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
        (nil . (mode-line . mode-line-inactive))))

;; Separators
(setq telephone-line-primary-left-separator 'telephone-line-cubed-right
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-right
      telephone-line-primary-right-separator 'telephone-line-cubed-left
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left)
(setq telephone-line-height 26)


;; Custom segments
(telephone-line-defsegment my-buffer-info-segment ()
  '(""
    mode-line-mule-info
    mode-line-modified
    mode-line-client
    mode-line-remote
    mode-line-frame-identification))
(telephone-line-defsegment my-buffer-name-segment ()
  (telephone-line-raw mode-line-buffer-identification t))

;; Based on https://github.com/dbordak/telephone-line/issues/68#issuecomment-393781601
(telephone-line-defsegment* my-vc-info-segment ()
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode)
        (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
          (concat (propertize (format "")
                              'face `(:foreground "yellow" :height 1.3))
                  (propertize (format " %s" branch)
                              'face `(:foreground "yellow" :height 0.9)))))
      ((string-match "SVN-" vc-mode)
        (let ((revision (cadr (split-string vc-mode "-"))))
          (concat (propertize (format "")
                              'face `(:height 1.3))
                  (propertize (format " %s" revision)
                              'face `(:height 0.9)))))
      (t (format "%s" vc-mode)))))

(require 'battery)
(display-battery-mode 1)
(setq battery-mode-line-format "%b%p%%%")
(telephone-line-defsegment my-battery-segment ()
  (telephone-line-raw
   (battery-format battery-mode-line-format (funcall battery-status-function))))

(defun update-battery-faces ()
  "Update the status of the battery in the mode line."
  (let ((battery-level (string-to-number (battery-format "%p" (funcall battery-status-function)))))
    (color-theme-sanityinc-solarized--with-colors 'dark
    (cond ((< battery-level battery-load-critical)
           (set-face-attribute 'battery-face nil :background red))
          ((< battery-level battery-load-low)
           (set-face-attribute 'battery-face nil :background orange))
          (t
           (set-face-attribute 'battery-face nil :background green))))))

(advice-add 'battery-update :after #'update-battery-faces)

;; Segments
(setq telephone-line-lhs
      '((nil . (my-buffer-info-segment))
        (buffer-name . (my-buffer-name-segment
                        telephone-line-process-segment))
        (major . (telephone-line-major-mode-segment))
        (minor . (telephone-line-minor-mode-segment))
        (vc . (my-vc-info-segment))
        (nil . (telephone-line-narrow-segment))))
(setq telephone-line-rhs
      '((nil . (telephone-line-narrow-segment))
        (battery . (my-battery-segment))
        (linum . (telephone-line-airline-position-segment))))

;; Turn on telephone-line
(telephone-line-mode t)

;; Reduce mode-line clutter
(require 'diminish)
(diminish 'flyspell-mode)
(diminish 'yas-minor-mode)
(diminish 'subword-mode)
(diminish 'auto-complete-mode)

(provide 'modeline)
;;; modeline.el ends here
