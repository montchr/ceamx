;;; misc.el --- Miscellaneous dead                   -*- lexical-binding: t; -*-

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

;;; Code:

;;
;;; tree-sitter
;;

;;  FIXME: make native tree-sitter support work... somehow... need langs installed...
;;  TODO: maybe try <https://github.com/nix-community/nix-doom-emacs/blob/9a5b34d9ba30842eb8f0d7deb08bf03a75930471/overrides.nix#L106-L111>

;; FIXME:
;; (use-feature! treesit
;;   :hook prog-mode)

;;
;;; pulsar :: pulse/highlight line on demand or after running select functions
;;  <https://protesilaos.com/emacs/pulsar>

;; TODO: remove? might be redundant with `evil-goggles'

;; (use-package pulsar
;;   :commands (pulsar-global-mode)
;;   :defer 5
;;   :config
;;   (setq pulsar-pulse           t)
;;   (setq pulsar-delay           0.055)
;;   (setq pulsar-iterations      10)
;;   (setq pulsar-face            'pulsar-magenta)
;;   (setq pulsar-highlight-face  'pulsar-yellow)
;;   (pulsar-global-mode 1))


(provide 'misc)
;;; misc.el ends here
