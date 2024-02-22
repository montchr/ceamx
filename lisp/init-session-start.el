;;; init-session-start.el --- Session startup customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

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

;; What happens when Emacs starts?

;; Here we configure things like `desktop-save-mode' and the dashboard.

;;; Code:

(require 'ceamx-paths)

(require 'lib-common)

(use-package dashboard
  :ensure t
  :after (nerd-icons)

  :preface
  (setopt initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  :init
  (add-hook 'ceamx-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'ceamx-after-init-hook #'dashboard-initialize)

  :config
  (setopt dashboard-banner-logo-title "C E A M X")
  (setopt dashboard-startup-banner 'official)
  (setopt dashboard-projects-backend 'project-el)
  (setopt dashboard-center-content t)
  (setopt dashboard-display-icons-p t)
  (setopt dashboard-icon-type 'nerd-icons)
  (setopt dashboard-items '((recents  . 5)
                             (bookmarks . 5)
                             (projects . 5)
                             (agenda . 5)
                             (registers . 5)))
  (dashboard-setup-startup-hook))

(provide 'init-session-start)
;;; init-session-start.el ends here
