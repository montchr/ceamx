;;; init-editor.el --- Editing --- -*- lexical-binding: t -*-

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

;;  Basic editor configuration

;;; Code:

;; Global indentation defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete) ; Indent, then try completions
(setq-default tab-width 2)
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

(use-feature emacs
  :init
  ;; TODO: is this redundant when `aggressive-indent-mode'?
  ;; (electric-indent-mode +1)
  (electric-pair-mode +1)
  (setq electric-pair-mode-preserve-balance nil)

  ;; mode-specific local-electric pairs
  ;; <https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:BE3F251D-5F39-4337-B27C-CFB81EE9A504>
  (defconst +default-electric-pairs electric-pair-pairs)
  ;; TODO: rename without slash, as this is non-interactive
  (defun cmx--add-local-electric-pairs (pairs)
    "Example usage:
    (add-hook 'jupyter-org-interaction-mode (lambda () (cmx--add-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append +default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  (defun cmx--org-mode--add-local-electric-pairs ()
    (cmx--add-local-electric-pairs '((?= . ?=)
                                     (?~ . ?~))))
  (add-hook 'org-mode-hook #'cmx--org-mode--add-local-electric-pairs))

(use-package editorconfig
  :commands (editorconfig-mode)
  :config (editorconfig-mode 1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; tree-sitter
;;
;;  FIXME: make native tree-sitter support work... somehow... need langs installed...
;;  TODO: maybe try <https://github.com/nix-community/nix-doom-emacs/blob/9a5b34d9ba30842eb8f0d7deb08bf03a75930471/overrides.nix#L106-L111>

(use-package tree-sitter)
(use-package tree-sitter-langs :after tree-sitter)

;; FIXME:
;; (use-feature treesit
;;   :hook prog-mode)


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; emacs-reformatter
;;  <https://github.com/purcell/emacs-reformatter>

(use-package reformatter
  :functions (reformatter-define))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; Pulsar :: Pulse highlight line on demand or after running select functions
;;  <https://protesilaos.com/emacs/pulsar>

(use-package pulsar
  :commands (pulsar-global-mode)
  :defer 5
  :config
  (setq pulsar-pulse           t)
  (setq pulsar-delay           0.055)
  (setq pulsar-iterations      10)
  (setq pulsar-face            'pulsar-magenta)
  (setq pulsar-highlight-face  'pulsar-yellow)
  (pulsar-global-mode 1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; hl-todo :: Highlight TODO and other codetags in comments and strings
;;  <https://github.com/tarsius/hl-todo>
;;  <https://peps.python.org/pep-0350/#specification>

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; aggressive-indent-mode :: Re-indent code after every change
;;  <https://github.com/Malabarba/aggressive-indent-mode>

;;; TODO: consider removing ... might be too aggressive

(use-package aggressive-indent
  :commands (global-aggressive-indent-mode)
  :defer 2
  :config
  (global-aggressive-indent-mode 1))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; vundo (visual undo) :: Visualize the undo tree.
;;  <https://github.com/casouri/vundo>

(use-package vundo
  :defer t
  :commands (vundo)
  :defines (vundo-unicode-symbols vundo-glyph-alist)
  :config
  (setq! vundo-glyph-alist vundo-unicode-symbols))


;; § ────────── ────────── ────────── ────────── ────────── ──────────
;;; drag-stuff :: Move stuff around in arbitrary directions
;;  <https://github.com/rejeep/drag-stuff.el>
;;
;; This package appears to be abandoned since 2017.
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
