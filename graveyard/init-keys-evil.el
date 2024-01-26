;;; init-keys-evil.el --- Evil keybindings initialization  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later AND MIT

;; Copyright (C) 2023-2024  Chris Montgomery
;; Copyright (C) 2014-2022  Henrik Lissner

;; Author: Chris Montgomery <chris@cdom.io>
;;         Henrik Lissner
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

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Initialization for evil-mode and its evil relatives.
;;
;; Largely stolen from Doom Emacs' base evil configurations.

;; TODO: add some of doom's evil keybindings <https://github.com/doomemacs/doomemacs/blob/master/modules/editor/evil/config.el#L403-L610>
;; TODO: check out <https://depp.brause.cc/dotemacs/#orgf271dd6>

;;;; References:

;; * <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/editor/evil/config.el>
;; * <https://github.com/bling/dotemacs/blob/97c72c8425c5fb40ca328d1a711822ce0a0cfa26/config/config-evil.el>

;;; Code:

;; FIXME: (require 'elpaca)

(require 'config-keys)
(require 'config-lisp)
(require 'lib-common)
(require 'lib-keys)


;;
;;; Configuration
;;

;; These variables really really do need to be set even before the
;; `use-package' `:init' keyword would allow, apparently. Otherwise,
;; everything is busted. `evil-collection', for one, complains about
;; `evil-want-keybinding' being set too late...

(defvar evil-want-integration t)
(defvar evil-want-keybinding nil)
;; C-h as backspace in insert mode.
(defvar evil-want-C-h-delete t)
;; Note that C-i bindings have some caveats for historical reasons.
;; This needs a workaround to prevent Emacs from conflating C-i and TAB.
(defvar evil-want-C-i-jump t)
(defvar evil-want-C-g-bindings nil)
(defvar evil-want-C-w-delete nil)
(defvar evil-want-Y-yank-to-eol t)

;; Results in too many unwanted changes from common Emacs bindings.
;; Instead, rebind some of evil's insert state bindings later.
(defvar evil-disable-insert-state-bindings t)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer.
(defvar evil-want-C-u-scroll t)
(keymap-global-set "C-M-u" 'universal-argument)

(defvar evil-ex-visual-char-range t)
(defvar evil-respect-visual-line-mode t)

(defvar evil-move-beyond-eol t)
(defvar evil-shift-width 2)
(defvar evil-want-fine-undo t)

  ;;;; completions/abbrevs
(defvar evil-complete-all-buffers nil)
(defvar evil-want-abbrev-expand-on-insert-exit nil)

  ;;;; search
;; Only do highlighting in selected window so that Emacs has less work
;; to do highlighting them all.
(defvar evil-ex-interactive-search-highlight 'selected-window)
(defvar evil-ex-search-vim-style-regexp t)
;; Global search by default.
(defvar evil-ex-substitute-global t)
(defvar evil-symbol-word-search t)

  ;;;; windows
(defvar evil-split-window-below t)
(defvar evil-vsplit-window-right t)

(use-package evil
  :demand t

  :autoload (evil-set-cursor-color evil-ex-define-cmd)

  :init
  ;; Depends on `evil' functions.
  (require 'lib-keys-evil)

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  (setq evil-mode-line-format 'nil)
  (setq evil-default-cursor #'+evil-default-cursor-fn)
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn))
  (setq evil-insert-state-cursor '(bar . 2))
  (setq evil-visual-state-cursor 'hollow)

  :config
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ceamx/save-and-kill-this-buffer)

  ;; Assign initial evil state to some specific major modes.
  ;; Note that this approach will not work for minor modes,
  ;; which need a state function added as a mode hook.
  (dolist (cell '((custom-mode . emacs)
                  (eshell-mode . emacs)
                  (term-mode . emacs)))
    (evil-set-initial-state (car cell) (cdr cell)))

  ;; Use evil search module.
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must.
  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)

  (after! [undo-fu]
    (setopt evil-undo-system 'undo-fu))

  (after! [helpful]
    (setopt evil-lookup-func #'helpful-at-point))

  (after! [eldoc]
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (after! [corfu]
    ;; Close candidate overlay upon entering normal state.
    ;; FIXME: causes overlay to close almost immediately even in insert mode
    ;; (add-hook 'evil-normal-state-entry-hook #'corfu-quit)

    ;; (remove-hook 'evil-normal-state-entry-hook #'corfu-quit)
    ;; FIXME: total breakage of evil normal state -- wrong number of args
    ;; (advice-add 'evil-normal-state :after #'corfu-quit)
    ;; (advice-remove 'evil-normal-state #'corfu-quit)

    )

  (evil-mode 1))

(when (fboundp 'elpaca-wait)
  (elpaca-wait))

;; FIXME: this is terrible -- must be loaded after evil -- needs autoloads internally
(require 'lib-keys-evil)

;;
;;; `evil-collection' :: <https://github.com/emacs-evil/evil-collection>
;;

(use-package evil-collection
  :after (evil)

  :config
  (setopt evil-collection-setup-minibuffer nil)

  (after! 'elpaca
    (setopt evil-collection-elpaca-want-g-filters nil))

  (setopt evil-collection-mode-list
          '( bookmark consult comint compile eldoc debug diff-hl diff-mode dired
             embark eldoc eww elfeed flycheck flymake grep help helpful
             ibuffer imenu info magit-section magit man
             markdown-mode org rg ripgrep tab-bar term vertico
             vterm wgrep which-key xref xwidget))

  (defun +evil-collection-setup-h (_mode mode-keymaps)
    "Remove any bindings to global prefix keys in MODE-KEYMAPS."
    (evil-collection-translate-key '(normal) mode-keymaps
      (kbd "SPC") nil
      (kbd "M-SPC") nil
      "," nil))
  (add-hook 'evil-collection-setup-hook #'+evil-collection-setup-h)

  (evil-collection-init))

;;
;;; `evil-nerd-commenter' :: <https://github.com/redguardtoo/evil-nerd-commenter>
;;

(use-package evil-nerd-commenter
  :after (evil)
  :commands (evilnc-default-hotkeys
             evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :config
  (evilnc-default-hotkeys))

;;
;;; `evil-matchit' :: <https://github.com/redguardtoo/evil-matchit>
;;
;;  Jump between matching tags and keywords.

(use-package evil-matchit
  :after evil
  :commands (evilmi-inner-text-object
             evilmi-outer-text-object
             evilmi-jump-items
             evilmi-init-plugins)
  :config
  (evilmi-init-plugins))

;;
;;; Surround and Embrace Evil Mode (SEEM)
;;
;;  <https://github.com/cute-jumper/embrace.el?tab=readme-ov-file#for-evil-surround-users>
;;  <https://github.com/cute-jumper/evil-embrace.el/tree/master?tab=readme-ov-file#why>

;; TODO: do we really need these? or is it a vim-emulation completionist thing?

;; FIXME: disable insertion of extra padding space by default, but allow the option to override
;;; `evil-surround' :: <https://github.com/emacs-evil/evil-surround/>
;; (use-package evil-surround
;;   :after evil
;;   :commands (global-evil-surround-mode
;;              evil-surround-edit
;;              evil-Surround-edit
;;              evil-surround-region)
;;   :config
;;   (keymap-set evil-visual-state-map "S" #'evil-surround-region)
;;   (global-evil-surround-mode +1))

;; FIXME: `embrace' and `evil-embrace' don't install properly
;; `embrace' :: <https://github.com/cute-jumper/embrace.el>
;; (use-package embrace
;;             ;; FIXME: :elpaca (embrace :host github :repo "cute-jumper/embrace.el")
;;  :commands (embrace-org-mode-hook
;;             embrace-ruby-mode-hook
;;             embrace-emacs-lisp-mode-hook))

;; `evil-embrace' :: <https://github.com/cute-jumper/evil-embrace.el>
;; (use-package evil-embrace

;;             ;; FIXME: :elpaca (evil-embrace :host github :repo "cute-jumper/evil-embrace.el")
;;  :after (evil embrace)

;;  :commands (embrace-add-pair
;;             embrace-add-pair-regexp
;;             evil-embrace-enable-evil-surround-integration)

;;  :init
;;  (defun +evil-embrace-lisp-mode-hook-h ()
;;    ;; Avoid `embrace-add-pair-regexp' because it would overwrite the default
;;    ;; `f' rule, which we want for other modes
;;    (push (cons ?f (make-embrace-pair-struct
;;                    :key ?f
;;                    :read-function #'+evil--embrace-elisp-fn
;;                    :left-regexp "([^ ]+ "
;;                    :right-regexp ")"))
;;          embrace--pairs-list))

;;  (defun +evil-embrace-angle-bracket-modes-hook-h ()
;;    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
;;      (set var (delq ?< evil-embrace-evil-surround-keys))
;;      (set var (delq ?> evil-embrace-evil-surround-keys)))
;;    (embrace-add-pair-regexp ?< "\\_<[a-z0-9-_]+<" ">" #'+evil--embrace-angle-brackets)
;;    (embrace-add-pair ?> "<" ">"))

;;  (add-hook 'org-mode-hook #'embrace-org-mode-hook)
;;  (add-hook 'ruby-mode-hook #'embrace-ruby-mode-hook)
;;  (add-hook 'emacs-lisp-mode-hook #'embrace-emacs-lisp-mode-hook)

;;  (dolist (mode ceamx-lisp-mode-list)
;;    (add-hook mode #'+evil-embrace-lisp-mode-hook-h))

;;  (dolist (mode '(typescript-mode rustic-mode c++-ts-mode))
;;    (add-hook mode #'+evil-embrace-angle-bracket-modes-hook-h))

;;  (after! [evil-surround]
;;    (evil-embrace-enable-evil-surround-integration))

;;  ;; TODO: find out whether this is still necessary?
;;  ;; HACK: This must be done ASAP, before embrace has a chance to
;;  ;;   buffer-localize `embrace--pairs-list' (which happens right after it calls
;;  ;;   `embrace--setup-defaults'), otherwise any new, global default pairs we
;;  ;;   define won't be in scope.
;;  ;; via <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/editor/evil/config.el#L257C1-L265C64>
;;  (defadvice! +evil--embrace-init-escaped-pairs-a (&rest args)
;;    "Add escaped-sequence support to embrace."
;;    :after #'embrace--setup-defaults
;;    (embrace-add-pair-regexp ?\\ "\\[[{(]" "\\[]})]" #'+evil--embrace-escaped
;;                            (embrace-build-help "\\?" "\\?")))

;;  :config
;;  (setopt evil-embrace-show-help-p t))

;;;
;;; `evil-escape' :: <https://github.com/emacsorphanage/evil-escape/>
;;
;; > Customizable key sequence to escape from insert state and
;; > everything else in Emacs.

(use-package evil-escape
  :blackout
  :after evil
  :commands evil-escape
  :hook (on-first-input . evil-escape-mode)

  :init
  (setopt evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (setopt evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode))
  (setopt evil-escape-key-sequence "jk")
  (setopt evil-escape-delay 0.15)

  :config
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook! 'evil-escape-inhibit-functions
    (defun +evil-inhibit-escape-in-minibuffer-fn ()
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p))))))


;;
;;; `evil-exchange' :: <https://github.com/Dewdrops/evil-exchange>
;;

(use-package evil-exchange
  :after evil
  :commands evil-exchange
  :config
  (add-hook! 'ceamx-escape-hook
    (defun +evil--escape-exchange-h ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t)))
  (evil-exchange-install))

;;
;;; `evil-ts' :: <https://github.com/foxfriday/evil-ts>
;;

;; TODO
;; (straight-use-package '(evil-ts :type git :host github :repo "foxfriday/evil-ts"))

;;
;;; `evil-visualstar' :: <https://github.com/bling/evil-visualstar>
;;
;; > Start a * or # search from the visual selection

(use-package evil-visualstar
  :after evil
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward))

;;
;;; Text object plugins
;;

(use-package exato
  :after evil
  :commands (evil-outer-xml-attr evil-inner-xml-attr))

;;
;;; `anzu' + `evil-anzu'
;;
;;  Display current and total search match info in modeline.

;; TODO: consider moving to modeline file
(use-package anzu
  :blackout
  :defer 10
  :config (global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

;;
;;; `evil-numbers' :: <https://github.com/juliapath/evil-numbers>
;;
;;  Increment and decrement literal numbers.

;; TODO: enable only in writable buffers
(use-package evil-numbers :after evil)

;;
;;; `evil-terminal-cursor-changer' :: <https://github.com/7696122/evil-terminal-cursor-changer>
;;

(use-package evil-terminal-cursor-changer
  :after evil
  :defer t
  :unless (display-graphic-p)
  :config
  (evil-terminal-cursor-changer-activate))

;;
;;; `evil-indent-plus' :: <https://github.com/TheBB/evil-indent-plus>
;;

(use-package evil-indent-plus :after evil)

;;
;;; `evil-lion' :: <https://github.com/edkolev/evil-lion>
;;

(use-package evil-lion
  :after evil
  :config
  (setopt evil-lion-squeeze-spaces nil) ;; default t
  (evil-lion-mode))

;;
;;; `evil-args' :: <https://github.com/wcsmith/evil-args>
;;
;; > Motions and text objects for delimited [e.g. function] arguments

(use-package evil-args :after evil)

;;
;;; `evil-traces' :: <https://github.com/mamapanda/evil-traces>
;;
;;  visual previews for some `evil-ex' commands
;;  port of <https://github.com/markonm/traces.vim>

(use-package evil-traces
  :after evil
  :config
  ;; FIXME: this writes to custom.el
  (evil-traces-use-diff-faces)
  (evil-traces-mode))

;;
;;; `evil-goggles' :: <https://github.com/edkolev/evil-goggles>
;;

(use-package evil-goggles
  :after evil
  :init
  ;; example:
  ;; (setopt evil-goggles-enable-paste nil)

  ;; list of all on/off variables, their default value is `t`:
  ;;
  ;; evil-goggles-enable-delete
  ;; evil-goggles-enable-change
  ;; evil-goggles-enable-indent
  ;; evil-goggles-enable-yank
  ;; evil-goggles-enable-join
  ;; evil-goggles-enable-fill-and-move
  ;; evil-goggles-enable-paste
  ;; evil-goggles-enable-shift
  ;; evil-goggles-enable-surround
  ;; evil-goggles-enable-commentary
  ;; evil-goggles-enable-nerd-commenter
  ;; evil-goggles-enable-replace-with-register
  ;; evil-goggles-enable-set-marker
  ;; evil-goggles-enable-undo
  ;; evil-goggles-enable-redo
  ;; evil-goggles-enable-record-macro

  :config
  (setopt evil-goggles-duration 0.200) ; default => 0.200

  ;; this variable affects "blocking" hints, for example when deleting
  ;; -- the hint is displayed, the deletion is delayed (blocked) until
  ;; the hint disappers, then the hint is removed and the deletion
  ;; executed; it makes sense to have this duration short
  (setopt evil-goggles-blocking-duration 0.050) ;; default is nil, i.e. use `evil-goggles-duration'

  ;; this variable affects "async" hints, for example when indenting
  ;; -- the indentation is performed with the hint visible, i.e. the
  ;; hint is displayed, the action (indent) is executed
  ;; (asynchronous), then the hint is removed, highlighting the result
  ;; of the indentation
  (setopt evil-goggles-async-duration 0.900) ;; default is nil, i.e. use `evil-goggles-duration'

  (evil-goggles-mode))

(provide 'init-keys-evil)
;;; init-keys-evil.el ends here
