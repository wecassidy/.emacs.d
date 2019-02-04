;;; wec-cmodes.el --- configuring /(Objective-)?C(++|#)?/

;; Copyright (C) 2018  Wesley Cassidy

;; Author: Wesley Cassidy <wec@wec-debian>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

;; Better default compile command - use a makefile if it exists, otherwise GCC/G++
(require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
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
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			     file))))))

(add-hook 'csharp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode t)
              (setq c-basic-offset 4)
              (setq tab-width 4)))

(provide 'wec-cmodes)
;;; wec-cmodes.el ends here
