;;; init-lang-markdown.el --- Markdown support customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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

;; FIXME: no asteriks pair

;;; Code:

(require 'config-env)
(require 'lib-common)
(require 'lib-editor)
(require 'lib-lang-markdown)

(defvar org-src-lang-modes)

;; TODO: why not?
;; (define-keymap :keymap markdown-mode-map
;; 	"C-c i l" #'markdown-insert-link
;; 	;; FIXME: pop latest kill
;; 	"C-c i q" #'markdown-insert-blockquote)

;;; `markdown-mode' :: <https://github.com/jrblevin/markdown-mode>
(use-package markdown-mode
  :commands (markdown-mode)
  :autoload (markdown-match-generic-metadata)

  :init
  (defvar markdown-enable-wiki-links t)
  (defvar markdown-italic-underscore t)
  (defvar markdown-asymmetric-header t)
  (defvar markdown-gfm-additional-languages '("sh"))
  (defvar markdown-make-gfm-checkboxes-buttons t)
  (defvar markdown-fontify-whole-heading-line t)

  ;; HACK Due to jrblevin/markdown-mode#578, invoking `imenu' throws a
  ;;      'wrong-type-argument consp nil' error if you use native-comp.
  ;;      <https://github.com/jrblevin/markdown-mode/issues/578>
  (defvar markdown-nested-imenu-heading-index (not (ignore-errors (native-comp-available-p))))

  (defvar markdown-command #'cmx-markdown-compile)

  ;; This is set to `nil' by default, which causes a wrong-type-arg error
  ;; when you use `markdown-open'. These are more sensible defaults.
  ;; TODO: make this a globally-usable variable, since it always holds true.
  (defvar
    markdown-open-command
    (cond (+sys-mac-p "open")
          (+sys-linux-p "xdg-open")))

  ;; A sensible and simple default preamble for markdown exports that
  ;; takes after the github asthetic (plus highlightjs syntax coloring).
  ;; via <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/lang/markdown/config.el>
  (defvar markdown-content-type "application/xhtml+xml")
  (defvar markdown-css-paths
    '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
      "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (defvar markdown-xhtml-header-content
    (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
      "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
      "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
      "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
      "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))

  (after! 'org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))

  :config
  (def-advice! cmx--disable-markdown-front-matter-fontification (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
<https://github.com/jrblevin/markdown-mode/issues/328#issuecomment-405361296>
and
<https://github.com/radian-software/radian/blob/b2fac3a615186f77de0bdc7e4f06e9aa46c222bb/emacs/radian.el#L3199-L3206>."
    (ignore (goto-char (point-max))))

  ;; Register code fence pair.
  (after! [smartparens]
    (require 'lib-editor)
    (cmx-sp-pair #'markdown-mode "```")))

(use-package evil-markdown
  ;; FIXME: :elpaca (evil-markdown :host github :repo "Somelauw/evil-markdown")
  :after (evil evil-collection markdown-mode)
  :commands (evil-markdown-mode)
  :defines (evil-markdown-mode-map)

  :init
  (add-hook 'markdown-mode-hook #'evil-markdown-mode)

  (evil-define-key '(normal) 'evil-markdown-mode-map
    (kbd "M-r") #'browse-url-of-file)

  (evil-define-key '(insert) 'evil-markdown-mode-map
    (kbd "M-*") #'markdown-insert-list-item
    (kbd "M-b") #'markdown-insert-bold
    (kbd "M-i") #'markdown-insert-italic
    (kbd "M--") #'markdown-insert-hr)

  (evil-define-key '(motion) 'evil-markdown-mode-map
    "[h" #'markdown-previous-visible-heading
    "]h" #'markdown-next-visible-heading
    "[p" #'markdown-promote
    "]p" #'markdown-demote
    "[l" #'markdown-previous-link
    "]l" #'markdown-next-link))

(provide 'init-lang-markdown)
;;; init-lang-markdown.el ends here
