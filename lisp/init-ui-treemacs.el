;;; init-ui-treemacs.el --- Treemacs configuration   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; <https://github.com/Alexander-Miller/treemacs>

;;; Code:

(require 'lib-ui-treemacs)

(use-package treemacs
  :defer t
  :config
  (setopt treemacs-collapse-dirs                   (if treemacs-python-executable 3 0))
  (setopt treemacs-deferred-git-apply-delay        0.5)
  (setopt treemacs-directory-name-transformer      #'identity)
  (setopt treemacs-display-in-side-window          t)
  (setopt treemacs-eldoc-display                   'simple)
  (setopt treemacs-file-event-delay                2000)
  (setopt treemacs-file-extension-regex            treemacs-last-period-regex-value)
  (setopt treemacs-file-follow-delay               0.2)
  (setopt treemacs-file-name-transformer           #'identity)
  (setopt treemacs-follow-after-init               t)
  (setopt treemacs-expand-after-init               t)
  (setopt treemacs-find-workspace-method           'find-for-file-or-pick-first)
  (setopt treemacs-git-command-pipe                "")
  (setopt treemacs-goto-tag-strategy               'refetch-index)
  (setopt treemacs-header-scroll-indicators        '(nil . "^^^^^^"))
  (setopt treemacs-hide-dot-git-directory          t)
  (setopt treemacs-indentation                     2)
  (setopt treemacs-indentation-string              " ")
  (setopt treemacs-is-never-other-window           t)
  (setopt treemacs-max-git-entries                 5000)
  (setopt treemacs-missing-project-action          'ask)
  (setopt treemacs-move-forward-on-expand          nil)
  (setopt treemacs-no-png-images                   t)
  (setopt treemacs-no-delete-other-windows         t)
  (setopt treemacs-project-follow-cleanup          t)
  ;; FIXME: should already be handled by `no-littering', but needs confirmation
  ;; treemacs-persist-file                    (expand-file-name "treemacs-persist" cmx-local-dir)
  (setopt treemacs-position                        'left)
  (setopt treemacs-read-string-input               'from-child-frame)
  (setopt treemacs-recenter-distance               0.1)
  (setopt treemacs-recenter-after-file-follow      nil)
  (setopt treemacs-recenter-after-tag-follow       nil)
  (setopt treemacs-recenter-after-project-jump     'always)
  (setopt treemacs-recenter-after-project-expand   'on-distance)
  (setopt treemacs-litter-directories              '("node_modules" "vendor" "result" ".venv" ".cask"))
  (setopt treemacs-project-follow-into-home        nil)
  (setopt treemacs-show-cursor                     nil)
  (setopt treemacs-show-hidden-files               t)
  (setopt treemacs-silent-filewatch                nil)
  (setopt treemacs-silent-refresh                  nil)
  (setopt treemacs-sorting                         'alphabetic-case-insensitive-asc)
  (setopt treemacs-select-when-already-in-treemacs 'move-back)
  (setopt treemacs-space-between-root-nodes        t)
  (setopt treemacs-tag-follow-cleanup              t)
  (setopt treemacs-tag-follow-delay                1.5)
  (setopt treemacs-text-scale                      nil)
  (setopt treemacs-user-mode-line-format           nil)
  (setopt treemacs-user-header-line-format         nil)
  (setopt treemacs-wide-toggle-width               70)
  (setopt treemacs-width                           35)
  (setopt treemacs-width-increment                 1)
  (setopt treemacs-width-is-initially-locked       t)
  (setopt treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  (when +sys-mac-p
    (treemacs-resize-icons 44))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))



  (treemacs-hide-gitignored-files-mode nil)


  :bind
  ;; TODO: add to new sidebar keymap in `init-keys'
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-tab-bar
  :if tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))


(provide 'init-ui-treemacs)
;;; init-ui-treemacs.el ends here
