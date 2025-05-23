;;; lib-lang-markdown.el --- Markdown helpers        -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: languages, local

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

;;

;;; Code:

;;;###autoload
(defun ceamx-markdown-compile (beg end output-buffer)
  "Compile Markdown with pandoc if available.
Returns process exit code."
  (when (executable-find "pandoc")
    (call-process-region beg end "pandoc" nil output-buffer nil
      "-f" "markdown"
      "-t" "html"
      "--highlight-style=pygments")))

(provide 'lib-lang-markdown)
;;; lib-lang-markdown.el ends here
