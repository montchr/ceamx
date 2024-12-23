;;; init-lang-markdown.el --- Markdown support customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords: local, languages

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

;;; Code:

;;; Requirements

(require 'ceamx-lib)

;;; Install ~markdown-mode~

;; <https://github.com/jrblevin/markdown-mode>

(package! markdown-mode
  (setopt markdown-enable-wiki-links t)
  (setopt markdown-italic-underscore t)
  (setopt markdown-asymmetric-header t)
  (setopt markdown-gfm-additional-languages '("sh"))
  (setopt markdown-make-gfm-checkboxes-buttons t)
  (setopt markdown-fontify-whole-heading-line t)

  ;; HACK Due to jrblevin/markdown-mode#578, invoking `imenu' throws a
  ;;      'wrong-type-argument consp nil' error if you use native-comp.
  ;;      <https://github.com/jrblevin/markdown-mode/issues/578>
  (setopt markdown-nested-imenu-heading-index (not (ignore-errors (native-comp-available-p))))

  ;; This is set to `nil' by default, which causes a wrong-type-arg error
  ;; when you use `markdown-open'. These are more sensible defaults.
  (setopt markdown-open-command (cond
                                 ((ceamx-host-macos-p) "open")
                                 ((ceamx-host-gnu-linux-p) "xdg-open")))

  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown))))

(with-eval-after-load 'markdown-mode
  (defvar markdown-mode-map)
  (declare-function markdown-match-generic-metadata "markdown-mode")
  (declare-function markdown-insert-link "markdown-mode")
  (declare-function markdown-insert-blockquote "markdown-mode")

  (define-keymap :keymap markdown-mode-map
    "C-c i l" #'markdown-insert-link
    "C-c i q" #'markdown-insert-blockquote)

  ;; <https://github.com/jrblevin/markdown-mode/issues/328#issuecomment-405361296>
  ;; <https://github.com/radian-software/radian/blob/b2fac3a615186f77de0bdc7e4f06e9aa46c222bb/emacs/radian.el#L3199-L3206>.
  (def-advice! +markdown-disable-front-matter-fontification-a (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block."
    (ignore (goto-char (point-max)))))

(provide 'init-lang-markdown)
;;; init-lang-markdown.el ends here
