;;; init-workspace.el --- Workspaces and perspectives  -*- lexical-binding: t; -*-

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

;; Overwork and perspipritaction.

;;; Code:

(autoload 'consult-customize "consult")
(autoload 'after! "lib-common" t)

(defvar cmx-buffer-keymap)
(defvar consult--source-buffer)
(defvar consult-buffer-sources)
;; (defvar persp-consult-source)

;; TODO: restore https://github.com/alphapapa/bufler.el

(use-package burly
  :elpaca (burly :host github :repo "alphapapa/burly.el")
  :init
  (burly-tabs-mode))

;; (use-package perspective
;;   :commands (persp-mode
;;              persp-list-buffers
;;              persp-switch-to-buffer*
;;              persp-kill-buffer*)
;;   :autoload (persp-state-save)

;;   :custom                     ;
;;   (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here

;;   :init
;;   (persp-mode)

;;   :config
;;   (keymap-global-set "<remap> <switch-to-buffer>" #'persp-switch-to-buffer*)
;;   (keymap-global-set "<remap> <kill-buffer>" #'persp-kill-buffer*)

;;   (define-keymap :keymap cmx-buffer-keymap
;;     "b" #'persp-switch-to-buffer*
;;     "B" #'persp-list-buffers
;;     "K" #'persp-kill-buffer*)

;;   ;; Persist the perspective sessions to storage.
;;   (add-hook 'kill-emacs-hook #'persp-state-save))

;; ;; Hide default buffer sources, showing only buffers within the current perspective.
;; ;; Note that you can still access list of all buffers in all perspectives by narrowing using prefix b.
;; (after! [perspective consult]
;;   (consult-customize consult--source-buffer :hidden t :default nil)
;;   (add-to-list 'consult-buffer-sources persp-consult-source))

;; ;;; `perspective'+`projectile' integration -- <https://github.com/bbatsov/persp-projectile>
;; (use-package persp-projectile
;;   :after (perspective projectile)
;;   :commands (projectile-persp-switch-project)
;;   :config
;;   (keymap-global-set "<remap> <projectile-switch-project>" #'projectile-persp-switch-project))

(provide 'init-workspace)
;;; init-workspace.el ends here
