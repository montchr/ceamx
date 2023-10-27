;;; init-keys-evil.el --- Evil keybindings initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Montgomery

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

;; FIXME: add copyright+license from doom

;; Initialization for evil-mode and its evil relatives.
;;
;; Largely stolen from Doom Emacs' base evil configurations.

;; TODO: add some of doom's evil keybindings <https://github.com/doomemacs/doomemacs/blob/master/modules/editor/evil/config.el#L403-L610>
;; TODO: add from <https://depp.brause.cc/dotemacs/#orgf271dd6>

;;;; References:

;; * <https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/editor/evil/config.el>
;; * <https://github.com/bling/dotemacs/blob/97c72c8425c5fb40ca328d1a711822ce0a0cfa26/config/config-evil.el>

;;; Code:

(require 'config-keys)
(require 'lib-common)
(require 'lib-keys)

(autoload 'elpaca-wait "elpaca")

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
  ;; TODO: is this defined? no?
  (setq evil-default-cursor '+evil-default-cursor-fn)
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn))
  (setq evil-insert-state-cursor '(bar . 2))
  (setq evil-visual-state-cursor 'hollow)

  :config
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'cmx/save-and-kill-this-buffer)

  ;; Make sure some modes start in Emacs state
  (dolist (mode '(custom-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
  ;; this itself, so we must.
  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)

  (after! [undo-fu evil]
    (setq evil-undo-system 'undo-fu))

  (after! [helpful]
    (setq evil-lookup-func #'helpful-at-point))

  (after! [eldoc]
    ;; Allow eldoc to trigger directly after changing modes
    (eldoc-add-command 'evil-normal-state
                       'evil-insert
                       'evil-change
                       'evil-delete
                       'evil-replace))

  (evil-mode 1))

;;
;;; `evil-collection' :: <https://github.com/emacs-evil/evil-collection>
;;

(use-package evil-collection
  :after (evil)

  :config
  (setq! evil-collection-elpaca-want-g-filters nil)
  (setq! evil-collection-setup-minibuffer nil)
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
;;; `evil-surround' + `evil-embrace'
;;
;;  <https://github.com/emacs-evil/evil-surround/>
;;  <https://github.com/cute-jumper/evil-embrace.el/>

;; TODO: bindings
(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1))

;; can be loaded independently of evil
(use-package embrace
  :commands (embrace-org-mode-hook
             embrace-ruby-mode-hook)
  :init
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'ruby-mode-hook 'embrace-ruby-mode-hook))

(use-package evil-embrace
  :after (evil embrace)

  :commands
  embrace-add-pair
  embrace-add-pair-regexp
  evil-embrace-enable-evil-surround-integration

  :init
  (after! [evil-surround]
    (evil-embrace-enable-evil-surround-integration))

  :config
  (setq evil-embrace-show-help-p nil))

;;
;;; `evil-escape' :: <https://github.com/emacsorphanage/evil-escape/>
;;
;; > Customizable key sequence to escape from insert state and
;; > everything else in Emacs.

(use-package evil-escape
  :after evil
  :commands evil-escape
  :hook (on-first-input . evil-escape-mode)

  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (setq evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode))
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-delay 0.15)

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
  (add-hook! 'cmx-escape-hook
    (defun +evil--escape-exchange-h ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t)))
  (evil-exchange-install))


;;
;;; `evil-quickscope' :: <https://github.com/blorbx/evil-quickscope>
;;
;;  subtle always-on hints for unique f/t jump targets
;;  port of <https://github.com/unblevable/quick-scope>
;;

;; via <https://github.com/PythonNut/quark-emacs/blob/4c2e39f4b7b2c545e55aaced7059af4636121d0e/modules/config-evil-modules.el>
(use-package evil-quickscope
  :after evil
  :unless (< (display-color-cells) 256)

  :config
  (setq evil-quickscope-word-separator " -./")

  (set-face-attribute 'evil-quickscope-first-face nil
                      :inherit nil)

  (if (display-graphic-p)
      (set-face-attribute 'evil-quickscope-second-face nil
                          :underline '(:style wave)
                          :inherit nil)
    (set-face-attribute 'evil-quickscope-second-face nil
                        :inherit nil))

  (define-advice evil-quickscope-update-overlays-bidirectional
      (:override () only-normal-state)
    "Update overlays in both directions from point."
    (evil-quickscope-remove-overlays)
    (when (memq evil-state '(normal motion))
      (evil-quickscope-apply-overlays-forward)
      (evil-quickscope-apply-overlays-backward)))

  (global-evil-quickscope-always-mode +1))

;;
;;; `evil-snipe' :: <https://github.com/hlissner/evil-snipe>
;;
;; > 2-char searching ala vim-sneak & vim-seek

;; TODO: keybindings
(use-package evil-snipe
  :after evil
  :commands (evil-snipe-local-mode evil-snipe-override-local-mode)
  :hook ((on-first-input . evil-snipe-override-mode)
         (on-first-input . evil-snipe-mode))
  :init
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-repeat-scope 'visible)
  (setq evil-snipe-char-fold t))


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

(use-package anzu
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
  (setq evil-lion-squeeze-spaces nil) ;; default t
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
  (evil-traces-use-diff-faces)
  (evil-traces-mode))

(provide 'init-keys-evil)
;;; init-keys-evil.el ends here
