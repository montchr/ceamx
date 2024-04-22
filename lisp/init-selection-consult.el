;;; init-selection-consult.el --- Configuration for Consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Chris Montgomery

;; Author: Chris Montgomery <chris@cdom.io>

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

;; "Consulting completing-read"
;;
;; <https://github.com/minad/consult>

;; <https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html>

;;; Code:

(require 'lib-common)

(use-package consult
  :commands ( consult-bookmark consult-buffer consult-buffer-other-frame consult-buffer-other-tab
              consult-buffer-other-window consult-compile-error consult-complex-command
              consult-fd consult-flymake consult-focus-lines consult-git-grep
              consult-global-mark consult-goto-line consult-history consult-imenu
              consult-imenu-multi consult-info consult-keep-lines consult-kmacro
              consult-line consult-line-multi consult-locate consult-man consult-mark
              consult-mode-command consult-outline consult-preview-at-point-mode
              consult-project-buffer consult-register consult-register-format consult-register-load
              consult-register-store consult-register-window consult-ripgrep consult-xref
              consult-yank-pop)

  :init

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  ;; Improve previews for `consult-register' and other register commands.
  (setopt register-preview-delay 0.5)
  (setopt register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Display xref locations with previews.
  (setopt xref-show-definitions-function #'consult-xref)
  (setopt xref-show-xrefs-function #'consult-xref)

  (global-keys!
    "C-c M-x" #'consult-mode-command
    ;; TODO: needs configuration? see `consult-mode-histories'
    ;; "C-c h"   #'consult-history
    "C-c K" #'consult-kmacro
    "C-c M" #'consult-man

    ;; FIXME: use a keymap for s prefix
    ;; "C-c s h"  #'consult-history
    ;; "C-c s m"  #'consult-mode-command
    ;; "C-c s k"  #'consult-kmacro

    "<remap> <Info-search>" #'consult-info

    "C-x M-:" #'consult-complex-command     ; orig. `repeat-complex-command'
    "C-x b" #'consult-buffer                ; orig. `switch-to-buffer'
    "C-x 4 b" #'consult-buffer-other-window ; orig. `switch-to-buffer-other-window'
    "C-x 5 b" #'consult-buffer-other-frame ; orig. `switch-to-buffer-other-frame'
    "C-x t b" #'consult-buffer-other-tab   ; orig. `switch-to-buffer-other-tab'
    "C-x r"  #'consult-bookmark            ; orig. `bookmark-jump'
    "C-x p b" #'consult-project-buffer     ; orig. `project-switch-to-buffer'

    ;; Custom M-# bindings for fast register access
    "M-#"    #'consult-register-load
    "M-'"    #'consult-register-store
    "C-M-#"  #'consult-register

    ;; Miscellaneous
    "M-y" #'consult-yank-pop            ; orig. `yank-pop'

    ;; M-g bindings (goto-map)
    "M-g e"  #'consult-compile-error
    "M-g f"  #'consult-flymake ;; Alternative: consult-flycheck
    "M-g g" #'consult-goto-line
    "M-g M-g" #'consult-goto-line
    "M-g o"  #'consult-outline ;; Alternative: consult-org-heading
    "M-g m"  #'consult-mark
    "M-g k"  #'consult-global-mark
    "M-g i"  #'consult-imenu
    "M-g I"  #'consult-imenu-multi

    ;; M-s bindings (search-map)
    "M-s d"  #'consult-fd               ; or `consult-find'
    "M-s D"  #'consult-locate
    "M-s g"  #'consult-ripgrep
    "M-s G"  #'consult-git-grep
    "M-s l"  #'consult-line
    "M-s L"  #'consult-line-multi
    "M-s k"  #'consult-keep-lines
    "M-s u"  #'consult-focus-lines)

;;;; ~isearch~ integration

  (keymap-global-set "M-s e" #'consult-isearch-history)

  ;; Needed by consult-line to detect isearch.
  (define-keymap :keymap isearch-mode-map
    "M-e" #'consult-isearch-history
    "M-s e" #'consult-isearch-history
    "M-s l" #'consult-line
    "M-s L" #'consult-line-multi)

;;;; minibuffer integration

  (keymap-set minibuffer-local-map "M-s" #'consult-history) ; orig. next-matching-history-element
  (keymap-set minibuffer-local-map "M-r" #'consult-history) ; orig. previous-matching-history-element


  :config

  (setopt consult-preview-key 'any)

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
    consult-theme :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    ;; :preview-key (kbd "M-.")
    :preview-key '(:debounce 0.4 any))

  ;; Configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setopt consult-narrow-key "<") ;; "C-+"

  ;; Make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'embark-prefix-help-command)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  (with-eval-after-load 'projectile
    (declare-function projectile-project-root "projectile")
    (setopt consult-project-function (lambda (_) (projectile-project-root)))))

;; <https://github.com/minad/consult#embark-integration>
(use-package embark-consult
  :after (embark consult)
  :init
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(provide 'init-selection-consult)
;;; init-selection-consult.el ends here
