;;; wec-csharp.el --- configuration for C# code          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wesley Cassidy

;; Author: Wesley Cassidy <wec@wec-debian>
;; Keywords:

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

;;

;;; Code:
(add-hook 'csharp-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode t)
              (setq c-basic-offset 4)
              (setq tab-width 4)))

(provide 'wec-csharp)
;;; wec-csharp.el ends here
