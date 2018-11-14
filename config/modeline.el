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
 (defface vc-face
   `((t (:foreground ,background :background ,green))) "")
 (defface linum-face
   `((t (:foreground ,background :background ,blue))) ""))

(setq telephone-line-faces
      '((buffer-info . (buffer-info-face . buffer-info-face))
        (buffer-name . (buffer-name-face . buffer-name-face))
        (major . (major-mode-face . major-mode-face))
        (vc . (vc-face . vc-face))
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

;; Segments
(setq telephone-line-lhs
      '((nil . (my-buffer-info-segment))
        (buffer-name . (my-buffer-name-segment
                        telephone-line-process-segment))
        (major . (telephone-line-major-mode-segment))
        (vc . (my-vc-info-segment))
        (nil . (telephone-line-narrow-segment))))
(setq telephone-line-rhs
      '((nil . (telephone-line-narrow-segment))
        (linum . (telephone-line-airline-position-segment))))

;; Turn on telephone-line
(telephone-line-mode t)

(provide 'modeline)
;;; modeline.el ends here
