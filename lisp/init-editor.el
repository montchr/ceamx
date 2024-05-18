;;; init-editor.el --- Editor customizations  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
;;; Code:

(require 'ceamx-keymaps)

(require 'config-editor)

(require 'ceamx-lib)
(require 'lib-editor)
(require 'lib-simple)
(dolist (cmd '(downcase-region
               scroll-left
               upcase-region))
  (put cmd 'disabled nil))
(keymap-global-set "C-c i" #'ceamx-insert-map)
(delete-selection-mode 1)
;; Yes, it may appear strange and archaic, and yet... I realized that I actually
;; do *prefer* this for readability.  And so, I am giving it a shot.  My
;; grandfather, who was an English teacher for most of the 20th century, would
;; be very proud.  Though even writing this paragraph has been difficult.

(setopt sentence-end-double-space t)

(keymap-global-set "M-Q" #'repunctuate-sentences)
(global-subword-mode -1)
(setopt comment-auto-fill-only-comments nil)

(def-hook! +prog-mode-auto-fill-comments-only-h ()
  'prog-mode-hook
  "Set `auto-fill-mode' to only fill comments when in programming modes."
  (setq-local comment-auto-fill-only-comments t))

(dolist (mode-hook '(prog-mode-hook text-mode-hook))
  (add-hook mode-hook #'auto-fill-mode))
(package! unfill
  (keymap-global-set "M-q" #'unfill-toggle))
(keymap-global-set "<remap> <default-indent-new-line>" #'ceamx/continue-comment)
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
(define-keymap :keymap ceamx-pairs-map
  "(" '("paren" . insert-pair)
  "[" '("square-b" . insert-pair)
  "{" '("curly-b" . insert-pair)
  "<" '("angle-b" . insert-pair)
  "'" '("s-quote" . insert-pair)
  "\"" '("d-quote" . insert-pair)
  "`" '("b-tick" . insert-pair)
  "_" '("u-score" . insert-pair)
  "*" '("star" . insert-pair)
  "=" '("equals" . insert-pair)
  ":" '("colon" . insert-pair))

(keymap-set ceamx-insert-map "P" ceamx-pairs-map)
(setopt require-final-newline t)
(add-hook 'prog-mode-hook #'whitespace-mode)

(setopt whitespace-style
        '(face
          tabs
          tab-mark
          trailing
          missing-newline-at-eof))
(setq-default indent-tabs-mode nil)
(setopt indent-tabs-mode nil)
(setopt backward-delete-char-untabify-method 'untabify)
(electric-indent-mode 1)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; <https://editorconfig.org>

(use-package editorconfig
  :commands (editorconfig-mode)
  :init
  (add-hook 'on-first-file-hook #'editorconfig-mode))
;; <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>

(defun +editorconfig-enforce-org-mode-tab-width-h (props)
  "Prevent `editorconfig' from changing `tab-width' in `org-mode'.
A \"tab-width\" of any value other than 8 is an error state in
org-mode, so it must not be changed.

PROPS is as in `editorconfig-after-apply-functions'."
  (when (and (gethash 'indent_size props)
             (derived-mode-p 'org-mode))
    (setq tab-width 8)))

(with-eval-after-load 'editorconfig
  (add-hook 'editorconfig-after-apply-functions
            #'+editorconfig-enforce-org-mode-tab-width-h))
(package! apheleia
  (apheleia-global-mode 1))
(after! (apheleia blackout)
  (blackout 'apheleia-mode ceamx-apheleia-lighter))
(def-advice! +apheleia-save-buffer-maybe-reformat-a (func &optional arg)
  :around #'save-buffer
  "Inhibit reformatting-on-save when providing a prefix argument to \\[save-buffer]."
  (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
    (funcall func)))
(after! (apheleia)
  (add-to-list 'apheleia-inhibit-functions #'+apheleia-format-maybe-inhibit-h))
(package! puni
  ;; (puni-global-mode)
  ;; (add-hook 'prog-mode-hook #'puni-mode)
  ;; (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  )

;; (after! puni
;;     ;; (define-keymap :keymap puni-mode-map
;;   ;;   "C-M-f" #'puni-forward-sexp
;;   ;;   "C-M-b" #'puni-backward-sexp
;;   ;;   "C-M-a" #'puni-beginning-of-sexp
;;   ;;   "C-M-e" #'puni-end-of-sexp
;;   ;;   "C-M-[" #'puni-backward-sexp-or-up-list
;;   ;;   "C-M-]" #'puni-forward-sexp-or-up-list

;;   ;;   "M-(" #'puni-syntactic-forward-punct
;;   ;;   "M-)" #'puni-syntactic-backward-punct
;;   ;;   )

;; )
(use-package drag-stuff
  :bind
  (([M-up] . drag-stuff-up)
   ([M-right] . drag-stuff-right)
   ([M-down] . drag-stuff-down)
   ([M-left] . drag-stuff-left)))
(use-feature! rect
  :config
  (use-feature! hydra
    :config
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
      ("g" nil))

    (when (fboundp 'hydra-rectangle/body)
      (keymap-global-set "C-x SPC" #'hydra-rectangle/body)
      (keymap-global-set "C-x M-r" #'rectangle-mark-mode))))
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
(package! easy-kill
  (keymap-global-set "M-w" #'easy-kill)   ; override `kill-ring-save'
  (keymap-global-set "C-M-@" #'easy-mark) ; override `mark-sexp'
  )
(package! ialign
  (keymap-global-set "C-x l" #'ialign))
(define-prefix-command 'ceamx-structural-editing-prefix)
(keymap-global-set "C-c s" #'ceamx-structural-editing-prefix)
(after! puni

  (defvar-keymap structural-editing-map
    :repeat t

    "d" #'puni-forward-delete-char
    ;; "DEL" #'puni-backward-delete-char
    ;; "D" #'puni-forward-kill-word
    ;; "M-DEL" #'puni-backward-kill-word
    ;; "C-k" #'puni-kill-line
    ;; "M-k" #'puni-backward-kill-line
    "k" #'kill-sexp

    "f" #'puni-forward-sexp
    "b" #'puni-backward-sexp
    "[" #'puni-backward-sexp-or-up-list
    "]" #'puni-forward-sexp-or-up-list
    "a" #'puni-beginning-of-sexp
    "e" #'puni-end-of-sexp
    "u" #'puni-up-list
    "M-(" #'puni-syntactic-forward-punct
    "M-)" #'puni-syntactic-backward-punct

    "\\" #'indent-region
    "/" #'undo

    ">" #'puni-slurp-forward
    "<" #'puni-slurp-backward
    "}" #'puni-barf-forward
    "{" #'puni-barf-backward
    "R" #'puni-raise
    "t" #'puni-transpose
    "C" #'puni-convolute
    ;; FIXME: avoid meow dependency -- no puni equivalent
    ;; "J" #'meow-join-sexp
    "S" #'puni-split
    ;; FIXME: for `emacs-lisp-mode' only
    "x" #'eval-defun

    ))

;; FIXME: wrong type argument symbolp
;; (map-keymap (lambda (_ cmd)
;;               (put cmd 'repeat-exit-timeout nil)) structural-editing-map)

(keymap-set ceamx-insert-map "d" #'ceamx/insert-date)

(define-keymap :keymap (current-global-map)
  "C-=" #'ceamx/insert-date
  "C-<" #'ceamx/escape-url-dwim

  ;; Logical progression from M-f for `forward-word'.
  ;; See also `forward-sexp'
  "M-F" #'forward-symbol)

(provide 'init-editor)
;;; init-editor.el ends here
