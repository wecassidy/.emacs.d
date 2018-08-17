;;; wec-cpp.el --- C++                                   -*- lexical-binding: t; -*-

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

;; Compile with g++ using all warnings and full optimization
(add-hook 'c++-mode-hook
          (lambda()
            (set (make-local-variable 'compile-command)
                 (concat "g++ -Wall -O3 "
                         (if buffer-file-name
                             (shell-quote-argument buffer-file-name))))))

(provide 'wec-cpp)
;;; wec-cpp.el ends here
