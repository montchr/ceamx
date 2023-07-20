;;; init-evil.el --- Evil configuration -*- lexical-binding: t; -*-

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

;; Configuration for evil-mode and its evil relatives.

;;; Code:

(use-package evil
  :demand t

  :preface
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)

  (defun cmx/save-and-kill-this-buffer () (interactive) (save-buffer) (kill-this-buffer))

  :custom
  (evil-symbol-word-search t "search by symbol with * and #.")
  (evil-shift-width 2 "Same behavior for vim's '<' and '>' commands")
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-complete-all-buffers nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-Y-yank-to-eol t)
  (evil-search-module 'evil-search "use vim-like search instead of 'isearch")
  (evil-select-search-module 'evil-search-module 'isearch)
  (evil-undo-system 'undo-redo) ; use emacs default
  (evil-kill-on-visual-paste t)
  (evil-respect-visual-line-mode t)
  (evil-normal-state-cursor 'box)
  (evil-visual-state-cursor 'hollow)
  (evil-insert-state-cursor '(bar . 2))
  (evil-emacs-state-cursor '(hbar . 2))
  (evil-ex-interactive-search-highlight 'selected-window)

  :config
  ;; Use default Emacs mouse click behavior
  (define-key evil-motion-state-map [down-mouse-1] nil)

  ;; Global search by default.
  (setq evil-ex-substitute-global t)

  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'cmx/+save-and-kill-this-buffer)

  (evil-mode 1))

(use-package evil-collection
  :after (evil)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-setup-minibuffer nil)
  :config
  (evil-collection-init))

(use-package anzu
  :defer 10
  :config (global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-commentary
  :after (evil)
  :config (evil-commentary-mode +1)
  :diminish)

(use-package evil-surround
  :after (evil)
  :general
  ( :states 'operator
    "s"  'evil-surround-edit
    "S"  'evil-Surround-edit)
  ( :states 'visual
    "S"   'evil-surround-region
    "gS"  'evil-Surround-region))

(provide 'init-evil)
;;; init-evil.el ends here
