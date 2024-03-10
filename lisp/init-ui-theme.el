;;; init-ui-theme.el --- Theme Initalization -*- lexical-binding: t -*-

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

;;  Load the theme configurations.

;;; Code:

;;; Requirements

(require 'config-env)
(require 'config-ui)
(require 'lib-ui-theme)

;;; General Configuration

;; Don't prompt to confirm theme safety.
(setopt custom-safe-themes t)

;;; Ensure themes are applied in new frames to prevent flashing

;; TODO: also some other link i can't find now
;; <https://protesilaos.com/emacs/dotemacs#h:7d3a283e-1595-4692-8124-e0d683cb15b2>
(add-hook 'after-make-frame-functions #'ceamx-theme-re-enable-in-frame)

;;; Add a custom hook to run after enabling a theme

;; via <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>

(defvar ceamx-after-enable-theme-hook nil)

(defun ceamx-after-enable-theme (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'ceamx-after-enable-theme-hook))

(advice-add 'enable-theme :after #'ceamx-after-enable-theme)

(provide 'init-ui-theme)
;;; init-ui-theme.el ends here
