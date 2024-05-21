;;; init-workspace.el --- Workspaces, activities, scopes, and other organizational closures  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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

(require 'ceamx-lib)
(defvar edebug-inhibit-emacs-lisp-mode-bindings)
;; FIXME: restoring bookmark results in stray mini childframe (like that old
;; embark issue)
(package! burly
  (require 'burly))
;; Prevent `edebug' default bindings from interfering.
(setq edebug-inhibit-emacs-lisp-mode-bindings t)

(keymap-global-unset "C-x C-a" t)
(keymap-global-set "C-x C-a" (cons "Activities" (define-prefix-command 'ceamx-activities-prefix)))

(package! activities
  (activities-mode)
  ;; Unfortunately, due to the `tab-bar-mode' display bug in Emacs 29 (see
  ;; `init-workspace'), I am avoiding `tab-bar-mode'.  Though I really would
  ;; like to use it, the bug is just too distracting and impossible to work
  ;; around.
  (unless (version< emacs-version "30.0")
    (when tab-bar-mode
      (activities-tabs-mode)))

  ;; TODO: still shares bindings with edebug which is confusing
  (define-keymap :keymap (current-global-map)
    "C-x C-a C-n" #'activities-new
    "C-x C-a C-d" #'activities-define
    "C-x C-a C-a" #'activities-resume
    "C-x C-a C-s" #'activities-suspend
    "C-x C-a C-k" #'activities-kill
    "C-x C-a RET" #'activities-switch

    "C-x C-a b" #'activities-switch-buffer
    "C-x C-a g" #'activities-revert
    "C-x C-a l" #'activities-list))
(package! breadcrumb
  (add-hook 'ceamx-after-init-hook #'breadcrumb-mode))

(provide 'init-workspace)
;;; init-workspace.el ends here
