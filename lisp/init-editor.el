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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(dolist (sym '(add-function advice-add plist-put))
  (put sym 'lisp-indent-function 2))

;; TODO: is this redundant when `aggressive-indent-mode'?
;; (electric-indent-mode +1)

(global-subword-mode -1)

(electric-pair-mode +1)

;;
;;; page-break-lines :: <https://github.com/purcell/page-break-lines>
;;

(use-package page-break-lines
  :defer 1
  :commands (global-page-break-lines-mode)
  :config
  (global-page-break-lines-mode))

;;
;;; editorconfig
;;

(use-package editorconfig
  :commands (editorconfig-mode)
  :config (editorconfig-mode 1))

;;
;;; emacs-reformatter :: <https://github.com/purcell/emacs-reformatter>
;;

(use-package reformatter
  :functions (reformatter-define))

;;
;;; tree-sitter
;;

;;  FIXME: make native tree-sitter support work... somehow... need langs installed...
;;  TODO: maybe try <https://github.com/nix-community/nix-doom-emacs/blob/9a5b34d9ba30842eb8f0d7deb08bf03a75930471/overrides.nix#L106-L111>

(use-package tree-sitter)
(use-package tree-sitter-langs :after tree-sitter)

;; FIXME:
;; (use-feature treesit
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

;;
;;; hl-todo :: <https://github.com/tarsius/hl-todo>
;;
;;  Highlight TODO and other codetags in comments and strings
;;
;;  <https://peps.python.org/pep-0350/#specification>

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;;
;;; aggressive-indent-mode :: <https://github.com/Malabarba/aggressive-indent-mode>
;;

(use-package aggressive-indent
  :commands (global-aggressive-indent-mode)
  :defer 2
  :config
  (global-aggressive-indent-mode 1))

;;
;;; undo-fu :: <https://codeberg.org/ideasman42/emacs-undo-fu>
;;
;;  Simple, stable linear undo with redo for Emacs.

(use-package undo-fu
  :config
  (keymap-global-unset "C-z")
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo))

;;
;;; undo-fu-session :: <https://codeberg.org/ideasman42/emacs-undo-fu-session>
;;
;;  Save & recover undo steps between Emacs sessions.

(use-package undo-fu-session
  :after undo-fu

  :init
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session" +path-var-dir))

  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-ignore-temp-files t)
  (setq undo-fu-session-ignore-encrypted-files t)

  (undo-fu-session-global-mode))

;;
;;; vundo (visual undo) :: <https://github.com/casouri/vundo>

(use-package vundo
  :defer t
  :commands (vundo)
  :defines (vundo-unicode-symbols vundo-glyph-alist)
  :config
  (setq! vundo-glyph-alist vundo-unicode-symbols))


;;
;;; drag-stuff :: <https://github.com/rejeep/drag-stuff.el>
;;
;;  Move stuff around in arbitrary directions
;;
;;  This package appears to be abandoned since 2017.
;;  But, as of <2023-09-06>, it still works well.
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

;;
;;; Rectangle operations
;;

(use-feature rect
  :after hydra
  :defer t

  :config
  ;; via <https://github.com/abo-abo/hydra/wiki/Rectangle-Operations#rectangle-2>
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :hint nil
                                       :post (deactivate-mark))
    "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
    ("k" rectangle-previous-line)
    ("j" rectangle-next-line)
    ("h" rectangle-backward-char)
    ("l" rectangle-forward-char)
    ("d" kill-rectangle)                    ;; C-x r k
    ("y" yank-rectangle)                    ;; C-x r y
    ("w" copy-rectangle-as-kill)            ;; C-x r M-w
    ("o" open-rectangle)                    ;; C-x r o
    ("t" string-rectangle)                  ;; C-x r t
    ("c" clear-rectangle)                   ;; C-x r c
    ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
    ("N" rectangle-number-lines)            ;; C-x r N
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)))
    ("u" undo nil)
    ("g" nil)))


(provide 'init-editor)
;;; init-editor.el ends here
