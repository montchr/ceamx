;;; init-projects.el --- Projects configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
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

;;  Configuration for project awareness.

;; TODO: consider whether we can get by with `project.el'

;;; Code:

(require 'config-projects)

;;
;;; projectile :: <https://github.com/bbatsov/projectile>
;;  <https://docs.projectile.mx>

(use-package projectile
  :init
  ;; Use Emacs default completion system i.e. `completing-read'
  (setopt projectile-completion-system 'default)

  (setopt projectile-project-root-files '(".envrc" ".projectile"))

  (projectile-mode +1)

  :config
  (define-keymap :keymap projectile-mode-map
    ;; NOTE: Overrides default `cmx-project-map' binding.
    "C-c p" '("project" . projectile-command-map))

  (keymap-global-set "<remap> <project-find-file>" #'projectile-find-file)


  ;; TODO: use `cmx-projects-dir'
  (setopt projectile-project-search-path '(("~/Developer/sources/" . 1)
                                           ("~/Developer/contrib/" . 2)
                                           ("~/Developer/work/" . 3)))
  (setopt projectile-require-project-root t)
  (setopt projectile-sort-order 'recently-active)
  (setopt projectile-switch-project-action #'projectile-find-file)

  (add-to-list 'projectile-globally-ignored-directories "vendor"))

(use-package treemacs-projectile
  :after (treemacs projectile))

(provide 'init-projects)
;;; init-projects.el ends here
