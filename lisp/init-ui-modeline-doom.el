;;; init-ui-modeline-doom.el --- doom-modeline init  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>
;; Keywords:

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

;;

;;; Code:

;; NOTE: be wary of performance issues, as this package is notoriously heavy
;; FIXME: darwin: icons worked inconsistently until installing
;; "Symbols Nerd Font Mono" via `nerd-icons-install-fonts'.
;; The resulting file =NFM.ttf= seems significantly different than the version
;; installed by Nix, but I'm not positive yet because the macOS Font Book app is
;; pretty difficult to work with.
;; Nerd Fonts recently released a new major version and I suspect that may factor into the issue.
;; Note, however, that the nerd-icons package uses the latest font version (v3.0.x)
;; This GitHub issue /might/ be relevant: <https://github.com/rainstormstudio/nerd-icons.el/issues/43>
(use-package doom-modeline
  :demand t

  :init
  (doom-modeline-mode 1)

  :config
  (setopt doom-modeline-support-imenu t)
  (setopt doom-modeline-unicode-fallback t)
  (setopt doom-modeline-buffer-encoding nil)
  (setopt doom-modeline-github nil)
  (setopt doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Enable HUD mode, providing a micromap of buffer position.
  (setopt doom-modeline-hud t)

  (setopt doom-modeline-icon t)

  ;; note that the major mode icon is not missing like most others.
  ;; git branch icon is also fine.
  (setopt doom-modeline-major-mode-icon t)

  ;; FIXME: missing icons when using nix-installed icon font
  (setopt doom-modeline-buffer-state-icon t)
  (setopt doom-modeline-buffer-modification-icon t)

  (setopt doom-modeline-modal t)
  ;; FIXME: missing icon with nix-installed font... but only when non-nil?! when nil, icon displays properly...
  (setopt doom-modeline-modal-icon t))

(provide 'init-ui-modeline-doom)
;;; init-ui-modeline-doom.el ends here
