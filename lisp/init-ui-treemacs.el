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
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0))
  (setq treemacs-deferred-git-apply-delay        0.5)
  (setq treemacs-directory-name-transformer      #'identity)
  (setq treemacs-display-in-side-window          t)
  (setq treemacs-eldoc-display                   'simple)
  (setq treemacs-file-event-delay                2000)
  (setq treemacs-file-extension-regex            treemacs-last-period-regex-value)
  (setq treemacs-file-follow-delay               0.2)
  (setq treemacs-file-name-transformer           #'identity)
  (setq treemacs-follow-after-init               t)
  (setq treemacs-expand-after-init               t)
  (setq treemacs-find-workspace-method           'find-for-file-or-pick-first)
  (setq treemacs-git-command-pipe                "")
  (setq treemacs-goto-tag-strategy               'refetch-index)
  (setq treemacs-header-scroll-indicators        '(nil . "^^^^^^"))
  (setq treemacs-hide-dot-git-directory          t)
  (setq treemacs-indentation                     2)
  (setq treemacs-indentation-string              " ")
  (setq treemacs-is-never-other-window           t)
  (setq treemacs-max-git-entries                 5000)
  (setq treemacs-missing-project-action          'ask)
  (setq treemacs-move-forward-on-expand          nil)
  (setq treemacs-no-png-images                   t)
  (setq treemacs-no-delete-other-windows         t)
  (setq treemacs-project-follow-cleanup          t)
  ;; FIXME: should already be handled by `no-littering', but needs confirmation
  ;; treemacs-persist-file                    (expand-file-name "treemacs-persist" +path-local-dir)
  (setq treemacs-position                        'left)
  (setq treemacs-read-string-input               'from-child-frame)
  (setq treemacs-recenter-distance               0.1)
  (setq treemacs-recenter-after-file-follow      nil)
  (setq treemacs-recenter-after-tag-follow       nil)
  (setq treemacs-recenter-after-project-jump     'always)
  (setq treemacs-recenter-after-project-expand   'on-distance)
  (setq treemacs-litter-directories              '("node_modules" "vendor" "result" ".venv" ".cask"))
  (setq treemacs-project-follow-into-home        nil)
  (setq treemacs-show-cursor                     nil)
  (setq treemacs-show-hidden-files               t)
  (setq treemacs-silent-filewatch                nil)
  (setq treemacs-silent-refresh                  nil)
  (setq treemacs-sorting                         'alphabetic-case-insensitive-asc)
  (setq treemacs-select-when-already-in-treemacs 'move-back)
  (setq treemacs-space-between-root-nodes        t)
  (setq treemacs-tag-follow-cleanup              t)
  (setq treemacs-tag-follow-delay                1.5)
  (setq treemacs-text-scale                      nil)
  (setq treemacs-user-mode-line-format           nil)
  (setq treemacs-user-header-line-format         nil)
  (setq treemacs-wide-toggle-width               70)
  (setq treemacs-width                           35)
  (setq treemacs-width-increment                 1)
  (setq treemacs-width-is-initially-locked       t)
  (setq treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

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

  (defvar-keymap cmx-explore-keymap
    "t" #'treemacs)


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

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar
  :if tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))


(provide 'init-ui-treemacs)
;;; init-ui-treemacs.el ends here
