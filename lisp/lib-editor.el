;;; lib-editor.el --- Editor library functions       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for general editor configuration.

;;; Sources:

;; - <https://github.com/radian-software/radian/blob/develop/emacs/radian.el#L2174-L2214>

;;; Code:

(autoload 'sp-local-pair "smartparens")

(defun cmx--smartparens-indent-new-pair (&rest _)
  "Insert an extra newline after point, and reindent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun cmx--smartparens-pair-setup (mode delim)
  "In major mode MODE, set up DELIM with `newline-and-indent'."
  (sp-local-pair mode delim nil :post-handlers
                 '((cmx--smartparens-indent-new-pair "RET")
                   (cmx--smartparens-indent-new-pair "<return>"))))

(provide 'lib-editor)
;;; lib-editor.el ends here
