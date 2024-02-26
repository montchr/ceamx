;;; init-workspace.el --- Workspaces and perspectives  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

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

;; FIXME: `tab-bar-mode' is currently broken due to upstream Emacs 29 bug
;; <https://lists.gnu.org/r/bug-gnu-emacs/2023-07/msg01594.html>
;; For this reason, avoid `burly-tabs-mode' on Emacs 29.


;;; Code:

(require 'lib-common)
(require 'lib-keys)

(defvar edebug-inhibit-emacs-lisp-mode-bindings)

;; FIXME: restoring bookmark results in stray mini childframe (like that old
;; embark issue)
(use-package burly
  :demand t
  :commands (burly-open-last-bookmark)
  :autoload (burly-bookmark-frames))

(use-package beframe
  :ensure t
  :demand t

  :preface
  (defface +beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defun +beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
    (declare-function beframe-buffer-names "beframe")
    (declare-function beframe-buffer-sort-visibility "beframe")
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  :config
  ;; FIXME: still listed as frame buffers
  (setopt beframe-global-buffers '("\\*scratch\\*" "\\*Messages\\*" "\\*Backtrace\\*"))

  (keymap-global-set "C-c b" beframe-prefix-map)
  (beframe-mode 1)

  (use-feature! consult
    (declare-function consult--buffer-state "consult")
    (defvar +beframe-consult-source
      `(:name "Frame-specific buffers (current frame)"
        :narrow ?F
        :category buffer
        :face +beframe-buffer
        :history beframe-history
        :items ,#'+beframe-buffer-names-sorted
        :action ,#'switch-to-buffer
        :state ,#'consult--buffer-state))
    (add-to-list 'consult-buffer-sources '+beframe-consult-source)))

;;; Introduce an `activities'-based workflow for frame/tab/window/buffer management

(use-package activities
  :commands ( activities-mode activities-tabs-mode activities-new activities-resume activities-suspend
              activities-kill activities-switch activities-revert activities-list)

  :preface
  (defmap! ceamx-activities-map)
    (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :init
  (activities-mode)

  ;; Unfortunately, due to the `tab-bar-mode' display bug in Emacs 29 (see
  ;; `init-workspace'), I will be leaving this disabled for the time being.
  ;; (unless tab-bar-mode
  ;;   (activities-tabs-mode))

  (keymap-global-set "C-x C-a" '("[ Activities ]"))

  (keys! ceamx-activities-map
    "C-n" #'activities-new
    "C-a" #'activities-resume
    "C-s" #'activities-suspend
    "C-k" #'activities-kill
    "RET" #'activities-switch

    "g" #'activities-revert
    "l" #'activities-list))

;; TODO: <https://github.com/alphapapa/ap.el/blob/0831e0bb603cf3fe1cdeaa9f1c97b02f681c1f74/init.el#L395>
;; (use-package bufler
;;   :ensure (:files (:defaults (:exclude "helm-bufler.el")))
;;   :config
;;   (global-keys!
;;     "C-x b" #'bufler-switch-buffer
;;     "C-x B" #'bufler-workspace-focus-buffer
;;     "C-x C-b" #'bufler)
;;   (setopt bufler-groups
;;     (bufler-defgroups
;;       (group (auto-workspace)))))

(provide 'init-workspace)
;;; init-workspace.el ends here
