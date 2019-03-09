;;; modeline.el --- Emacs mode line

;;; Commentary:
;; Sets up telephone-line, defines solarized colour constants

;;; Code:
(require 'telephone-line)

;; Faces
(require 'color-theme-sanityinc-solarized)
(color-theme-sanityinc-solarized--with-colors
 'dark
 (defface buffer-name-face
   `((t (:foreground ,background :background ,magenta))) "")
 (defface major-mode-face
   `((t (:foreground ,background :background ,blue))) "")
 (defface minor-mode-face
   `((t (:foreground ,background :background ,violet))) "")
 (defface vc-face
   `((t (:foreground ,background :background ,green))) "")
 (defface linum-face
   `((t (:foreground ,background :background ,blue))) ""))

(defun set-modeline-faces (solarized-mode)
  "Advice to set the modeline faces when switching from light to dark.
`solarized-mode' will contain the color scheme function, so
checking it determines light or dark."
  (color-theme-sanityinc-solarized--with-colors
   solarized-mode
   (set-face-attribute 'buffer-name-face nil
                        :foreground background)
   (set-face-attribute 'major-mode-face nil
                        :foreground background)
   (set-face-attribute 'minor-mode-face nil
                        :foreground background)
   (set-face-attribute 'vc-face nil
                        :foreground background)
   (set-face-attribute 'linum-face nil
                       :foreground background)))

(defun set-modeline-faces-dark () (set-modeline-faces 'dark))
(defun set-modeline-faces-light () (set-modeline-faces 'light))

(advice-add 'color-theme-sanityinc-solarized-light :after #'set-modeline-faces-light)
(advice-add 'color-theme-sanityinc-solarized-dark :after #'set-modeline-faces-dark)

(setq telephone-line-faces
      '((buffer-name . (buffer-name-face . buffer-name-face))
        (major . (major-mode-face . major-mode-face))
        (minor . (minor-mode-face . minor-mode-face))
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
(telephone-line-defsegment my-buffer-name-segment ()
  (concat
   (telephone-line-raw mode-line-buffer-identification t)
   (telephone-line-raw mode-line-modified t)))

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
      '((buffer-name . (my-buffer-name-segment
                        telephone-line-process-segment))
        (major . (telephone-line-major-mode-segment))
        (minor . (telephone-line-minor-mode-segment))
        (vc . (my-vc-info-segment))
        (nil . (telephone-line-narrow-segment))))
(setq telephone-line-rhs
      '((nil . (telephone-line-narrow-segment))
        (linum . (telephone-line-airline-position-segment))))

;; Turn on telephone-line
(telephone-line-mode t)

;; Reduce mode-line clutter
(require 'diminish)
(diminish 'flyspell-mode)
(diminish 'yas-minor-mode)
(diminish 'subword-mode)
(diminish 'auto-complete-mode)
(diminish 'highlight-indent-guides-mode)

(provide 'modeline)
;;; modeline.el ends here
