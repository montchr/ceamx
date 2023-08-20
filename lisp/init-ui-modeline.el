;;; init-ui-modeline.el --- Modeline -*- lexical-binding: t -*-

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

;;  Modeline configuration

;;; Code:

(display-time-mode -1)
(setq column-number-mode t)

(use-package diminish)
;; Wait until `diminish` is activated so its use-package keyword is installed
(elpaca-wait) 

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
  :after (nerd-icons)
  :hook (elpaca-after-init . doom-modeline-mode)
  :config
  (setq! doom-modeline-support-imenu t)
  (setq! doom-modeline-unicode-fallback t)
  (setq! doom-modeline-buffer-encoding nil)
  (setq! doom-modeline-github nil)

  (setq! doom-modeline-icon t)

  ;; note that the major mode icon is not missing like most others.
  ;; git branch icon is also fine.
  (setq! doom-modeline-major-mode-icon t)

  ;; FIXME: missing icons when using nix-installed icon font
  (setq! doom-modeline-buffer-state-icon t)
  (setq! doom-modeline-buffer-modification-icon t)

  (setq! doom-modeline-modal t)
  ;; FIXME: missing icon with nix-installed font... but only when non-nil?! when nil, icon displays properly...
  (setq! doom-modeline-modal-icon t))

(provide 'init-ui-modeline)
;;; init-ui-modeline.el ends here
