;;; init-projects.el --- Projects configuration -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;; Code:

;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; projectile :: Project Interaction Library for Emacs
;;  <https://github.com/bbatsov/projectile>
;;  <https://docs.projectile.mx>

(elpaca-use-package projectile
  ;; :demand
  :after (general)

  :general
  (+general-global-project
    "a" '(projectile-add-known-project :which-key "add")
    "f" '(projectile-find-file-dwim :which-key "find-file-dwim")
    "i" '(projectile-invalidate-cache :which-key "re-cache")
    "p" '(projectile-switch-project :which-key "switch..."))
  (+general-global-search
    "p" '(projectile-ripgrep :which-key "project...")
    "x" '(projectile-find-references :which-key "xref"))

  ;; TODO: maybe, but mostly not -- see <https://docs.projectile.mx/projectile/usage.html#using-projectile-with-project-el>
  ;; ([remap project-find-file] . #'projectile-find-file
  ;;  [remap save-some-buffers] . #'projectile-save-project-buffers
  ;;  [remap other-buffer]      . #'projectile-project-buffers-other-buffer)

  ( :keymaps 'projectile-mode-map
    "s-p"    'projectile-command-map
    "C-c p"  'projectile-command-map)

  :init
  ;; Use Emacs default completion system i.e. `completing-read'
  (setq projectile-completion-system 'default)
  ;; TODO: should this be set here entirely, or merely appended-to?
  (setq projectile-project-root-files '(".envrc" ".projectile"))
  (projectile-mode +1)

  :config
  (setq projectile-project-search-path '("~/Developer/sources/"
                                         "~/Developer/contrib/"
                                         "~/Developer/work/projects/"))
  (setq projectile-require-project-root t)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-switch-project-action #'projectile-find-file))

(provide 'init-projects)
;;; init-projects.el ends here
