;;; init-editor.el --- Editing --- -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Basic editor configuration

;;; Code:

(use-feature emacs
  :init
  ;; (electric-indent-mode +1)
  (electric-pair-mode +1)
  (setq electric-pair-mode-preserve-balance nil)

  ;; mode-specific local-electric pairs
  ;; <https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:BE3F251D-5F39-4337-B27C-CFB81EE9A504>
  (defconst +default-electric-pairs electric-pair-pairs)
  (defun cmx/add-local-electric-pairs (pairs)
    "Example usage:
    (add-hook 'jupyter-org-interaction-mode '(lambda () (cmx/add-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append +default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  :hook
  ((org-mode . (lambda () (cmx/add-local-electric-pairs '((?= . ?=)
                                                          (?~ . ?~)))))))

(use-package editorconfig
  :commands (editorconfig-mode)
  :config (editorconfig-mode 1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; tree-sitter
;;
;;  Since builtin tree-sitter support is only available in Emacs 29+ (still in
;;  development as of <2023-01-28>), the internal documentation might be the
;;  best resource for describing how it can be used. In general, though, it
;;  seems as though support must be added by a combination of individual major
;;  modes (probably won't happen until well after Emacs 29 is released...) and
;;  the availability of language parser definitions (of which very few are
;;  included in Emacs 29 currently...).
;;
;;  <https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29>
;;
;;  So, for all those reasons... ignore the builtin tree-sitter support for now,
;;  and use the external package instead...

(use-package tree-sitter)
(use-package tree-sitter-langs :after tree-sitter)

;; FIXME: nope.
;; (use-feature treesit
;;   :hook prog-mode)


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; Apheleia :: Run code formatter on buffer contents without moving point
;;  <https://github.com/radian-software/apheleia>

(use-package apheleia
  :init (apheleia-global-mode +1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; Pulsar :: Pulse highlight line on demand or after running select functions
;;  <https://protesilaos.com/emacs/pulsar>

(use-package pulsar
  :defer 5
  :config
  (setq pulsar-pulse           t)
  (setq pulsar-delay           0.055)
  (setq pulsar-iterations      10)
  (setq pulsar-face            'pulsar-magenta)
  (setq pulsar-highlight-face  'pulsar-yellow)
  (pulsar-global-mode 1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; aggressive-indent-mode :: Re-indent code after every change
;;  <https://github.com/Malabarba/aggressive-indent-mode>

(use-package aggressive-indent
  :commands (global-aggressive-indent-mode)
  :defer 2
  :config
  (global-aggressive-indent-mode 1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; drag-stuff :: Move stuff around in arbitrary directions
;;  <https://github.com/rejeep/drag-stuff.el>
;;
;; As of [2023-07-20], this package appears to be abandoned. , the last update was
;; in 2017.
;;
;;;; Issues
;;
;; Note that as of [2023-07-20] there are numerous warnings about deprecated functions in
;; recent versions of Emacs:
;;
;; <https://github.com/rejeep/drag-stuff.el/issues/36>
;;
;;;; Alternatives
;;
;; I haven't yet found any other package to move arbitrary regions up/down while
;; preserving column position.
;;
;; `move-text-mode' <https://github.com/emacsfodder/move-text>, claims to do
;; this but fails pretty badly, moving the region/selection to the first column
;; regardless of its original position.

(use-package drag-stuff
  :bind
  (([M-up]    . drag-stuff-up)
   ([M-right] . drag-stuff-right)
   ([M-down]  . drag-stuff-down)
   ([M-left]  . drag-stuff-left)))

(provide 'init-editor)
;;; init-editor.el ends here
