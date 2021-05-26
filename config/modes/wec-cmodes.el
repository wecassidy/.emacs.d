;;; wec-cmodes.el --- configuring /(Objective-)?C(++|#)?/
;;; Commentary:
;;; Code:

;; Indent style
(c-set-offset 'arglist-intro '+)
(c-set-offset 'arglist-close 0)

;; Better default compile command - use a makefile if it exists, otherwise GCC/G++
(add-hook 'c-mode-hook
          (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -o %s %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-Wall -g")
			     file))))))

(add-hook 'c++-mode-hook
          (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s %s %s %s"
                             (or (getenv "CCC") "g++")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-Wall -g")
			     file))))))

(add-hook 'csharp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode t)
              (setq c-basic-offset 4)
              (setq tab-width 4)))

;; GDB
(setq gdb-many-windows t)
(setq gdb-show-main t)
(add-hook 'gdb-mode-hook (lambda () (company-mode -1))) ; Disable company in GDB

(provide 'wec-cmodes)
;;; wec-cmodes.el ends here
