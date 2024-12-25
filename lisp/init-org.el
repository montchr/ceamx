;;; init-org.el --- Org-Mode support  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'ceamx-paths)
(require 'ceamx-lib)
(defvar org-directory ceamx-agenda-dir)

;; TODO: I would prefer to check for the directory's existence explicitly --
;; this feels strange at the top-level.
(f-mkdir-full-path org-directory)

(setopt org-agenda-files ceamx-default-agenda-files)
(package! doct
  (require 'doct))

(with-eval-after-load 'doct
  (declare-function doct "doct")

  (setopt org-capture-templates
          (doct `(("Inbox"
                   :keys "t"
                   ;; TODO: make sure this icon spec is up to date with 2024
                   :icon ("checklist" :set "octicon" :color "green")
                   ;; TODO: should this be evaled/expanded?
                   :file ceamx-default-capture-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))))))
(package! org-ql)
(package! org-contrib
  (after! org
    (require 'org-choose)
    (add-to-list 'org-modules 'org-choose)))
(setopt org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
;; Instead of forcing this always, use the function
;; `org-insert-heading-respect-content' directly, bound to [C-<return>].
(setopt org-insert-heading-respect-content nil)

(setopt org-M-RET-may-split-line nil)
(setopt org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))

(setopt org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(package! org-modern
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
(add-hook 'org-mode-hook #'prettify-symbols-mode)

(setopt org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil)

(setopt org-hide-emphasis-markers t)
(setopt org-link-descriptive t)

(package! org-appear
  (add-hook 'org-mode-hook #'org-appear-mode))
(setopt org-cycle-emulate-tab t)
(setopt org-indent-indentation-per-level 2)
(setopt org-startup-folded 'content)

;; Avoid unnecessary indentation effects unless specified in file header.
(setopt org-startup-indented nil)

;; Ensure leading stars are replaced by spaces.
(setopt org-modern-hide-stars "   ")
;; (setopt org-modern-hide-stars nil)
(setopt org-modern-star nil)

(setopt org-ellipsis "…")
;; (setopt org-ellipsis " ⇢") ; prefix is nbsp
(after! org
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))
(setopt org-log-done 'time)
(setopt org-todo-keywords
        '((sequence
           "TODO(t)"
           "INPRG(i@/!)"
           "BLOCKED(b@)"
           "HOLD(h@)"
           "PROJ(p)"
           "|"
           "DONE(d!)"
           "CANCELLED(x@/!)")))
(after! (org pulsar)
  (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-center)
    (add-hook hook #'pulsar-reveal-entry)))
(setopt org-agenda-tags-column 0)
(setopt org-agenda-block-separator ?─)
(setopt org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setopt org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
(package! org-super-agenda
  ;; FIXME: probably can use autoloads instead
  (require 'org-super-agenda))
(setopt org-auto-align-tags nil)
(setopt org-tags-column 0)

(setopt org-fold-catch-invisible-edits 'show-and-error) ; default: smart
(setopt org-refile-targets `((,ceamx-default-todo-file . (:level . 2))
                             ;; (org-agenda-files . (:maxlevel . 1))
                             (,(locate-user-emacs-file "TODO.org") . (:level . 1))
                             (nil . (:maxlevel . 5))))

(setopt org-refile-use-outline-path 'file)
(setopt org-outline-path-complete-in-steps nil)
(setopt org-refile-allow-creating-parent-nodes 'confirm)
(setopt org-refile-use-cache nil)

;; TODO: move this setting elsewhere
(setopt org-reverse-note-order t)       ; prepend new notes
(setopt org-image-actual-width 300)
(setopt org-startup-with-inline-images t)
(package! org-download
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable))
(defun +org-mode--local-set-tab-width-h ()
  "Set the `tab-width' in `org-mode' buffers to 8 columns.
Any `tab-width' value other than 8 will result in an error.

This should be set as late as possible, after all other
`org-mode-hook' functions added by packages and
configurations.  Hence the use of `after-change-major-mode-hook',
which runs at the very end of major-mode activation.

Intended for use as a local hook function on
`after-change-major-mode-hook' as added within `org-mode-hook'."

  ;; This check is necessary to handle, for example, `org-edit-src-code', which
  ;; clones the `org-mode' buffer and changes its major-mode.
  (when (derived-mode-p 'org-mode)
    (setq tab-width 8)))

(def-hook! +org-mode-enforce-tab-width-h ()
  'org-mode-hook
  "Add a local hook to control `tab-width' on `after-change-major-mode-hook'."
  (add-hook 'after-change-major-mode-hook #'+org-mode--local-set-tab-width-h 0 t))
(package! org-web-tools
  (keymap-set org-mode-map "C-c i l" #'org-web-tools-insert-link-for-url))
(package! org-sidebar)
(package! org-bookmark-heading
  (after! org
    (require 'org-bookmark-heading)))
(package! ox-gfm
  (after! org
    (require 'ox-gfm)))
;; Changing the indentation of source code is unhelpful and destructive.
(setopt org-edit-src-content-indentation 0)

(setopt org-edit-src-persistent-message nil)
(setopt org-src-fontify-natively t)
(setopt org-src-preserve-indentation t)
(setopt org-src-tab-acts-natively t)
;; TODO: current window when narrow/short frame, but otherwise reorganize-frame is good
;; (setopt org-src-window-setup 'current-window)
(setopt org-src-window-setup 'other-window)
(setopt org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t)
                                   (sql . t)))
(def-advice! +org-babel-load-language-on-demand-a (orig-fun &rest args)
  :around #'org-babel-execute-src-block
  "Load language if needed before executing a source block."
  (let ((language (org-element-property :language (org-element-at-point))))
    (unless (cdr (assoc (intern language) org-babel-load-languages))
      (add-to-list 'org-babel-load-languages (cons (intern language) t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    (apply orig-fun args)))
(package! (auto-tangle-mode
           :host github
           :repo "progfolio/auto-tangle-mode.el")
  (autoload 'auto-tangle-mode "auto-tangle-mode"))
(package! org-rich-yank
  (with-eval-after-load 'org-download
    (require 'org-rich-yank)

    (keymap-set org-mode-map "C-M-y" #'org-rich-yank)))

(provide 'init-org)
;;; init-org.el ends here
