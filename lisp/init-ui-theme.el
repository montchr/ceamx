;;; init-ui-theme.el --- Theme Initalization -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

(require 'config-ui)
(require 'lib-ui-theme)

;; Don't prompt to confirm theme safety.
;; This also has the benefit of avoiding problems with
;; first-time startup on Emacs > 26.3.
(setopt custom-safe-themes t)

;; Ensure that themes will be applied in new frames to counteract default
;; effects from early-init frame flash workaround.
;; TODO: also some other link i can't find now
;; <https://protesilaos.com/emacs/dotemacs#h:7d3a283e-1595-4692-8124-e0d683cb15b2>
(add-hook 'after-make-frame-functions #'cmx-theme-re-enable-in-frame)

;; Set up `after-enable-theme-hook'.
;; via <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
(defvar cmx-after-enable-theme-hook nil)
(defun cmx-after-enable-theme-hook (&rest _args)
  "Hook to run after enabling theme."
  (run-hooks 'cmx-after-enable-theme-hook))
(advice-add 'enable-theme :after #'cmx-after-enable-theme-hook)

;; TODO: why? also move to `init-ui-font'
(setopt font-lock-maximum-decoration t)

(when +sys-mac-p
  ;; `undecorated-round' is macOS-specific.
  (add-to-list 'default-frame-alist '(undecorated-round . t)))


(provide 'init-ui-theme)
;;; init-ui-theme.el ends here
