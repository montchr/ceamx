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

;;
;;; Formatting
;;

;; Default indentation: 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Automatically maintain indentation.
(electric-indent-mode +1)

;; Don't consider camelCaseWORDs as separate words.
(global-subword-mode -1)


;;; editorconfig :: <https://editorconfig.org>
(use-package editorconfig
  :commands (editorconfig-mode)
  :config (editorconfig-mode 1))

;;; emacs-reformatter :: <https://github.com/purcell/emacs-reformatter>
(use-package reformatter
  :config
  ;; Prettier is a commonly-used formatter for several languages.
  (reformatter-define prettier :program "prettier"))

;; TODO: <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/config/default/config.el#L83>
;; TODO: TAB back and forth between parens in normal state
;;       chatgpt tells me that smartparens is responsible for that in doom emacs, we shall see...
;;       ok i just installed doom in a scratch directory and weirdly enough i'm not seeing the behavior i remember at all
;;       but hey, it would be nice, right?
;;; `smartparens' :: <https://github.com/Fuco1/smartparens>
(use-package smartparens
  :config
  ;; Load default package configuration.
  (require 'smartparens-config)

  (setopt sp-base-key-bindings 'paredit)
  (setopt sp-autoskip-closing-pair 'always)
  (setopt sp-hybrid-kill-entire-symbol nil)

  (sp-use-paredit-bindings)

  (show-smartparens-global-mode +1)

  ;; TODO: this is already bound by default. why the alternative command?
  ;; TODO: rename prefix etc
  ;; (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  ;; FIXME: pick terminal friendly binding
  ;; (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  ;; (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))

  ;; Load `smartparens' just about everywhere editable.
  ;; FIXME: avoid constructing names -- obscures usages
  (dolist (mode '(prog-mode text-mode markdown-mode))
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook #'smartparens-mode))))

;;
;;; Visual feedback
;;

;;; hl-todo :: <https://github.com/tarsius/hl-todo>
;;  Highlight TODO and other codetags in comments and strings
;;  <https://peps.python.org/pep-0350/#specification>
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

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
