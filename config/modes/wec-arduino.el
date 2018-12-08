;;; wec-arduino.el --- arduino-mode configuration
;;; Commentary:
;;; Code:
;; `arduino-mode' doesn't inherit from `prog-mode', so buffer-local
;; modes have to be added on their own
(add-hook 'arduino-mode-hook (lambda ()
                            (subword-mode)
                            (flyspell-prog-mode)
                            (auto-complete-mode)))

(provide 'wec-arduino)
;;; wec-arduino.el ends here
