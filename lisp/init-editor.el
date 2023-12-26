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

(require 'lib-common)
(require 'lib-editor)

;; Don't consider camelCaseWORDs as separate words.
(global-subword-mode -1)

;; Default indentation: 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Automatically maintain indentation while typing.
(electric-indent-mode +1)
(setopt backward-delete-char-untabify-method 'hungry)

(defun cmx-prog-mode--init-defaults-h ()
  "Intialize common defaults for programming modes.
In most cases, these modes derive from `prog-mode', but there may be some exceptions."
  (whitespace-mode 1)
  (after! [smartparens evil]
    ;;; Maintain indentation and comments upon newline.
    ;; I first tried `keymap-local-set' but the binding was overridden by `evil-ret'.
    ;; TODO: advise `evil-ret'?
    (keymap-set evil-insert-state-map "RET" #'comment-indent-new-line)))

(defvar cmx-prog-mode-init-hook #'cmx-prog-mode--init-defaults-h)

(add-hook 'prog-mode-hook (cmd! (run-hooks 'cmx-prog-mode-init-hook)))

;;; `whitespace-mode' (internal)
;;
;;  Show all problematic whitespace as configured by `whitespace-style'.
;;
;;  This mode is buffer-local. It might be undesireable in some cases,
;;  so consider enabling it selectively.
(use-feature! whitespace
  :defines ( whitespace-style
             whitespace-display-mappings)
  :config
  (setopt whitespace-style '(face tabs tab-mark trailing))
  ;; Visualize tabs as a pipe character - "|" (ASCII ID 124)
  ;; NOTE: the original value is still present at tail
  (cl-pushnew '(tab-mark 9 [124 9] [92 9]) whitespace-display-mappings))


;;
;;; Packages
;;

;;; editorconfig :: <https://editorconfig.org>
(use-package editorconfig
  :config (editorconfig-mode +1))

;;; `snap-indent' :: <https://github.com/jeffvalk/snap-indent>
;;  yet another "simple automatic indentation" package
(use-package snap-indent
  :after (editorconfig)
  :init (add-hook 'prog-mode #'snap-indent-mode)
  :config
  (setopt snap-indent-format 'untabify)
  (setopt snap-indent-on-save t)
  ;; Prevent issues with long lines with this totally arbitrary number.
  ;; TODO: consider so-long
  (setopt snap-indent-length-limit 200))

;;; emacs-reformatter :: <https://github.com/purcell/emacs-reformatter>
(use-package reformatter
  :config
  ;; Prettier is a commonly-used formatter for several languages.
  (reformatter-define prettier :program "prettier"))

;; TODO: TAB back and forth between parens in normal state
;;       chatgpt tells me that smartparens is responsible for that in doom emacs, we shall see...
;;       ok i just installed doom in a scratch directory and weirdly enough i'm not seeing the behavior i remember at all
;;       but hey, it would be nice, right?
;;; `smartparens' :: <https://github.com/Fuco1/smartparens>
(use-package smartparens
  :autoload ( sp-local-pair)
  :commands ( smartparens-mode
              sp-use-paredit-bindings
              show-smartparens-global-mode)
  :init
  ;; Load `smartparens' just about everywhere editable.
  (dolist (mode '(prog-mode text-mode markdown-mode git-commit-mode))
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook #'smartparens-mode)))

  :config
  ;; Load default package configuration.
  (require 'smartparens-config)

  (setopt sp-base-key-bindings 'paredit)
  (setopt sp-autoskip-closing-pair 'always)
  (setopt sp-hybrid-kill-entire-symbol nil)

  (sp-use-paredit-bindings)

  (show-smartparens-global-mode +1)

  ;;; When pressing RET after inserting a pair, add an extra newline and indent.
  ;;  <https://github.com/radian-software/radian/blob/develop/emacs/radian.el#L2174-L2214>
  ;;  <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (dolist (delim '("(" "[" "{"))
    ;; NOTE: For some reason, the modes must be specified explicitly; that is,
    ;; specifying `fundamental-mode' does not imply all modes will inherit its
    ;; pairs.
    (dolist (mode '(fundamental-mode
                    javascript-mode
                    nix-mode
                    prog-mode
                    text-mode))
      (cmx-sp-pair mode delim)))

  (cmx-sp-pair #'python-mode "\"\"\"")

  ;; Work around https://github.com/Fuco1/smartparens/issues/1036.
  (when (fboundp 'minibuffer-mode)
    (sp-local-pair #'minibuffer-mode "`" nil :actions nil)
    (sp-local-pair #'minibuffer-mode "'" nil :actions nil))

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setopt sp-escape-quotes-after-insert nil))

;;
;;; Visual feedback
;;

;;; hl-todo :: <https://github.com/tarsius/hl-todo>
;;  Highlight TODO and other codetags in comments and strings
;;  <https://peps.python.org/pep-0350/#specification>
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Highlight current line.
(global-hl-line-mode +1)

;;; `rainbow-delimiters' :: <https://github.com/Fanael/rainbow-delimiters>
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode))

;;
;;; Mutations
;;

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

(use-feature! rect
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
