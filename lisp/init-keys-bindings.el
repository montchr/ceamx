;;; init-keys-bindings.el --- Keybindings            -*- lexical-binding: t; -*-

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

;; FIXME: turns out, unsurprisingly, that loading this last means that its
;; keymaps are unavailable for binding in any prior-loaded file, but i guess
;; that's kind of a given for this all-in-one-place approach to bindings

;; TODO: quikgrok descriptions for `ceamx-window-map' defs
;; TODO: vim-like case invert! <https://gitlab.com/slotThe/dotfiles/-/blob/77393d030021a3524c03f22bbb4a4ca75965a9fd/emacs/.config/emacs/lisp/keybindings.el#L79-92>

;;; Code:

(require 'ceamx-keymaps)
(require 'ceamx-paths)

(require 'config-env)
(require 'config-help)
(require 'config-keys)

(require 'lib-common)
(require 'lib-keys)


;;; Mouse/trackpad support

;;;; Horizontal scroll support (without scrollbars)

(keymap-global-set "<wheel-left>" #'scroll-left)
(keymap-global-set "<wheel-right>" #'scroll-right)

;;;; Buffers

(require 'lib-buffer)

;; Remove the default binding to `elisp-byte-compile-buffer' since we are not
;; byte-compiling init files at this time.
(keymap-unset emacs-lisp-mode-map "C-c C-b" t)

(define-keymap :keymap (current-global-map)
  "C-c [" #'previous-buffer
  "C-c ]" #'next-buffer
  "C-c b" #'scratch-buffer)

;;;; Code

;; TODO: consider this as part of `init-prog'

;; TODO: consider something more mnemonic?
(keymap-set goto-map "c" #'xref-find-definitions)

(keymap-global-set "M-C" ceamx-code-map)

(define-keymap :keymap ceamx-code-map
  "a" '("action.." . eglot-code-actions)
  "d" #'xref-find-definitions
  "r" '("rename..." . eglot-rename))

;;;; Files

(keymap-global-set "C-c f" '("[ File ]" . ceamx-file-map))
(keymap-global-set "C-c C-f" '("[ File ]" . ceamx-file-map))

(define-keymap :keymap ceamx-file-map
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name

  "c" '("copy..." . ceamx/copy-this-file)
  "d" '("delete" . ceamx/delete-this-file)
  "f" '("find (g)..." . find-file)
  "r" '("rename/move..." . ceamx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file)

  "C-d" '("diff with..." . ceamx/diff-with-file))

;;;; Help

(define-keymap :keymap help-map
  "c" #'helpful-callable
  "C" #'helpful-command
  "f" #'helpful-function
  "F" #'describe-face
  "h" #'helpful-at-point
  "i" #'consult-info                    ; overrides default `info' bind
  "k" #'helpful-key
  "K" #'describe-key-briefly
  "l" #'find-library
  "o" #'helpful-symbol
  "s" #'suggest
  ;; FIXME: no lambda binding
  "t" `("text-props (pt)" . ,(cmd!!
                               #'describe-text-properties
                               current-prefix-arg
                               (point)))
  "v" #'helpful-variable

  ;; Parity with the corresponding unmodded keys.
  ;; Primarily for Meow keypad, but also sometimes feels more natural to keep
  ;; holding Ctrl anyway.
  "C-k" #'helpful-key
  "C-o" #'helpful-symbol


  ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
  "C-h" nil)

;;;; Launcher

(keymap-global-set "C-c C-o" '("[ Launch ]" . ceamx-launch-map))
(keymap-global-set "C-c o" '("[ Launch ]" . ceamx-launch-map))

;; (define-keymap :keymap ceamx-launch-map)

;;;; Toggles

(keymap-global-set "C-c C-t" '("[ Toggle ]" . ceamx-toggle-map))
(keymap-global-set "C-c t" '("[ Toggle ]" . ceamx-toggle-map))

(defmap! ceamx-toggle-map
  "l" #'display-line-numbers-mode
  "w" '("side windows" . window-toggle-side-windows))

;;;; Window

(define-keymap :keymap (current-global-map)
  ;; I mistakenly hit this sequence frequently instead of C-x C-f, but have never
  ;; once needed to configure `fill-column' on-demand (that should be configured
  ;; explicitly, or simply call `set-fill-column' with M-x).
  "C-x f" #'find-file

  "C-x o" #'ace-window

  "C-x =" #'balance-windows             ; prev: C-x +
  ;; TODO: rebind
  "C-x +" nil)

;;; Leader Bindings

(leader-key! "`"   '("other buffer" . mode-line-other-buffer))

;; TODO: install
;; (leader-key! "?"   #'consult-apropos)

(after! [evil]
  ;; Bind leader key to existing leader map.
  (evil-define-key* '(normal visual motion) 'global
    (kbd ceamx-leader-key) 'mode-specific-command-prefix)
  (after! [magit]
    (keymap-set magit-mode-map
      ceamx-leader-key #'mode-specific-command-prefix))

  ;; Bind leader to `,' (comma).
  (evil-define-key* '(normal visual motion) 'global
    "," 'mode-specific-command-prefix))

;;; Global Bindings

;; TODO: hydra/transient?
;; Wrap text in supported symbols.
(dolist (pair '("[" "{" "\"" "'" "`"))
  (let ((key (format "M-%s" pair)))
    (keymap-global-set key #'insert-pair)))

;; macOS muscle-memory habits
(when +sys-mac-p
  (define-keymap :keymap (current-global-map)
    "s-x" #'kill-region
    "s-c" #'kill-ring-save
    "s-s" #'save-buffer
    "s-w" #'kill-buffer
    "s-{" #'tab-previous
    "s-}" #'tab-next))

(define-keymap :keymap (current-global-map)
  "C-'" #'avy-goto-char-2

  ;; "C-'" #'avy-goto-char-2

  "M-j" #'avy-goto-char-2
  ;; Logical progression from M-f for `forward-word'.
  "M-F" #'forward-symbol

  ;; FIXME: does not trigger autoload from use-package
  "C-x u" #'vundo
  ;; TODO: find a better place -- shadows default for `rectangle-mark-mode'
  ;; "C-x SPC" #'hydra-rectangle/body
  )

(provide 'init-keys-bindings)
;;; init-keys-bindings.el ends here
