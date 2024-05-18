;;; init-dashboard.el --- Dashboard support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Chris Montgomery

;; Author: Chris Montgomery <chmont@proton.me>
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

;; <https://github.com/emacs-dashboard/emacs-dashboard>

;;; Code:

(require 'ceamx-paths)

(require 'ceamx-lib)

(use-package dashboard
  :ensure t
  :demand t
  :after (nerd-icons)

  :preface
  (setopt initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  :init
  (add-hook 'ceamx-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'ceamx-after-init-hook #'dashboard-initialize)
  ;; HACK: Work around <https://github.com/emacs-dashboard/emacs-dashboard/issues/499>
  ;; (dashboard-setup-startup-hook)
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)
  (add-hook 'window-setup-hook #'dashboard-resize-on-hook)

  :config
  (setopt dashboard-banner-logo-title "C E A M X")
  (setopt dashboard-startup-banner 'official)
  (setopt dashboard-projects-backend 'project-el)
  (setopt dashboard-center-content t)
  (setopt dashboard-display-icons-p t)
  (setopt dashboard-icon-type 'nerd-icons)
  ;; NOTE: This value results in a warning because `dashboard-items' specifies an
  ;; incorrect `:type'. The value should be an alist, not a list of alists. At
  ;; the time of writing, the value is copied directly from the package README.
  ;; <https://github.com/emacs-dashboard/emacs-dashboard/issues/489>
  (setopt dashboard-items '((recents  . 5)
                             (bookmarks . 5)
                             (projects . 5)
                             (agenda . 5)
                             (registers . 5))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
