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

(use-package burly
  :demand t
  :commands (burly-open-last-bookmark)
  :autoload (burly-bookmark-frames))

;; TODO: for `burly' -- moved outside of declaration to avoid scary closing parens
;; :init
;; FIXME: `tab-bar-mode' is currently broken due to upstream Emacs 29 bug
;; <https://lists.gnu.org/r/bug-gnu-emacs/2023-07/msg01594.html>
;; (burly-tabs-mode)

;; FIXME: ugh no don't even try ... wait why? come on past-me, what do you mean?
;; Restore previous configuration during init.
;;   (add-hook 'on-init-ui-hook #'burly-open-last-bookmark)

;;   (def-hook! +burly-bookmark-frames-on-kill-emacs-h () kill-emacs-hook
;;              "Bookmark current frames and windows with `burly-bookmark-frames'
;; upon ending the Emacs session."
;;     (burly-bookmark-frames "ceamx-burly-default")))

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
