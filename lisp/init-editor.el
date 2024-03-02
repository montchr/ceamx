;;; init-editor.el --- Editing --- -*- lexical-binding: t -*-

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

;; Configuration applicable across all editable buffers.

;;; Investigate:

;; TODO: <https://github.com/tarsius/killer/blob/ace0547944933440384ceeb5876b1f68c082d540/killer.el>

;;; Code:

(require 'config-editor)
(require 'lib-common)
(require 'lib-editor)

;;
;;; Formatting

(use-feature! emacs
  :config

;;;; auto-fill-mode

  ;; When a mode defines a comment syntax, then only wrap those comments. In all
  ;; other modes (primarily `text-mode' derivatives), activating
  ;; `auto-fill-mode' will apply to all lines.
  (setopt comment-auto-fill-only-comments t)

  (dolist (mode-hook '(prog-mode-hook text-mode-hook))
    (add-hook mode-hook #'auto-fill-mode))

;;;; comments

  (keymap-global-set "<remap> <default-indent-new-line>" #'ceamx/continue-comment)

;;;;; Maintain indentation and comments upon newline.

  ;; (after! 'evil
  ;;   (when (boundp 'evil-insert-state-map)
  ;;     (keymap-set evil-insert-state-map
  ;;                 "RET" #'comment-indent-new-line)))

  ;; FIXME: does not have intended effect -- more like the opposite?
  ;;        `lispy-mode' overrides to `lispy-newline-and-indent-plain'
  ;; (after! 'meow
  ;;   (when (boundp 'meow-insert-state-keymap)
  ;;     (keymap-set meow-insert-state-keymap
  ;;       "RET" #'comment-indent-new-line)))

;;;; indentation

  (setq-default indent-tabs-mode nil)
  (setopt indent-tabs-mode nil)
  (setopt backward-delete-char-untabify-method 'untabify)

  (electric-indent-mode 1)

;;;; pair handling :: `(info "Matching")'

  (setopt blink-matching-paren t)
  ;; Avoid "expression" style, which looks too much like a selected region.
  (setopt show-paren-style 'parenthesis)

  (setopt electric-pair-preserve-balance t)
  (setopt electric-pair-delete-adjacent-pairs t)
  (setopt electric-pair-skip-whitespace t)
  ;; TODO: evaluating...
  (setopt electric-pair-open-newline-between-pairs t)

  (electric-pair-mode 1)
  (show-paren-mode 1)

;;;; symbols

  ;; Don't consider camelCaseWORDs as separate words.
  (global-subword-mode -1)

;;;; whitespace indicators

  ;;  Show all problematic whitespace as configured by `whitespace-style'.

  ;;  This mode is buffer-local. It might be undesireable in some cases, so enable
  ;;  it selectively.

  (add-hook 'prog-mode-hook #'whitespace-mode)

  (setopt whitespace-style '(face tabs tab-mark trailing))

  (when (boundp 'whitespace-display-mappings)
    ;; Visualize tabs as a pipe character - "|" (ASCII ID 124)
    ;; NOTE: the original value is still present at tail
    ;; TODO: use character constants for clarity
    (cl-pushnew '(tab-mark 9 [124 9] [92 9]) whitespace-display-mappings)))

;;
;;; Packages

;;;; editorconfig :: <https://editorconfig.org>

(use-package editorconfig
  :commands (editorconfig-mode)
  :init
  (add-hook 'on-first-file-hook #'editorconfig-mode))

;;;; `topsy.el' :: <https://github.com/alphapapa/topsy.el>

;;  "Simple sticky header showing definition beyond top of window"

;; TODO: remove or use -- it seems more confusing than helpful though.

(use-package topsy
  :disabled
  :commands (topsy-mode)
  :init
  (add-hook 'prog-mode-hook #'topsy-mode)
  (add-hook 'magit-section-mode-hook #'topsy-mode))

;;;; Apply code reformatting with `apheleia'

;; <https://github.com/radian-software/apheleia>

;; In case you run into issues with `web-mode' not updating syntax highlighting
;; after formatting (or other arbitrary modifications):
;; <https://github.com/doomemacs/doomemacs/blob/35dc13632b3177b9efedad212f2180f69e756853/modules/editor/format/config.el#L74-L83>

(use-package apheleia
  :blackout ceamx-apheleia-lighter
  :preface

  (defun +apheleia-format-maybe-inhibit-h ()
    "Check if formatting should be disabled for current buffer."
    (or (eq major-mode 'fundamental-mode)
        (string-blank-p (buffer-name))
        (eq ceamx-format-on-save-disabled-modes t)
        (not (null (memq major-mode ceamx-format-on-save-disabled-modes)))))

  :init
  (apheleia-global-mode 1)

  ;; via <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L2266-L2270>
  (def-advice! +apheleia-save-buffer-maybe-reformat-a (func &optional arg)
    :around #'save-buffer
    "Inhibit reformatting-on-save when providing a prefix argument to \\[save-buffer]."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  ;; "We need to do this both before and after Apheleia is loaded
  ;; because the autoloading is set up such that the minor mode
  ;; definition is evaluated twice."
  ;; <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L2272C1-L2275>
  (blackout 'apheleia-mode ceamx-apheleia-lighter)

  :config
  (add-to-list 'apheleia-inhibit-functions #'+apheleia-format-maybe-inhibit-h))

;;;; `puni' :: <https://github.com/AmaiKinono/puni>

;;  Structured editing (soft deletion, expression navigating & manipulating)
;;  that supports many major modes out of the box.

(use-package puni
  :commands (puni-global-mode
             puni-disable-puni-mode
             puni-backward-sexp-or-up-list
             puni-forward-sexp-or-up-list)

  :init
  (define-keymap :keymap puni-mode-map
    "C-M-[" #'puni-backward-sexp-or-up-list
    "C-M-]" #'puni-forward-sexp-or-up-list)

  ;; (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;;;; drag-stuff :: <https://github.com/rejeep/drag-stuff.el>

;;  Move stuff around in arbitrary directions

;;  This package appears to be abandoned since 2017.
;;  But, as of <2023-09-06>, it still works well.

;;;;; Issues

;; Note that as of [2023-07-20] there are numerous warnings about deprecated functions in
;; recent versions of Emacs:

;; <https://github.com/rejeep/drag-stuff.el/issues/36>

;;;;; Alternatives

;; I haven't yet found any other package to move arbitrary regions up/down while
;; preserving column position.

;; `move-text-mode' <https://github.com/emacsfodder/move-text>, claims to do
;; this but fails pretty badly, moving the region/selection to the first column
;; regardless of its original position.

(use-package drag-stuff
  :bind
  (([M-up] . drag-stuff-up)
   ([M-right] . drag-stuff-right)
   ([M-down] . drag-stuff-down)
   ([M-left] . drag-stuff-left)))

;;
;;; Rectangle operations

(use-feature! rect
  :config
  (use-feature! hydra
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
      ("d" kill-rectangle)               ;; C-x r k
      ("y" yank-rectangle)               ;; C-x r y
      ("w" copy-rectangle-as-kill)       ;; C-x r M-w
      ("o" open-rectangle)               ;; C-x r o
      ("t" string-rectangle)             ;; C-x r t
      ("c" clear-rectangle)              ;; C-x r c
      ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
      ("N" rectangle-number-lines)            ;; C-x r N
      ("r" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)))
      ("u" undo nil)
      ("g" nil))))

;;; Add support for Sublime-like multi-cursor editing with `multiple-cursors'

;; <https://github.com/magnars/multiple-cursors.el>

(use-package multiple-cursors
  :demand t
  ;; :autoload (mc/num-cursors)

  :config

  (use-feature! hydra
    :config

    ;; TODO: convert to transient
    (defhydra hydra-multiple-cursors (:hint nil)
      "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("|" mc/vertical-align)
      ("s" mc/mark-all-in-region-regexp :exit t)
      ("0" mc/insert-numbers :exit t)
      ("A" mc/insert-letters :exit t)
      ("<mouse-1>" mc/add-cursor-on-click)
      ;; Help with click recognition in this hydra
      ("<down-mouse-1>" ignore)
      ("<drag-mouse-1>" ignore)
      ("q" nil))))



(provide 'init-editor)
;;; init-editor.el ends here
