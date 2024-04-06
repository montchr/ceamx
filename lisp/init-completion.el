;;; init-completion.el --- Completion-At-Point enhancements  -*- lexical-binding: t;  -*-

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
;;; Code:

(require 'lib-completion)
(require 'lib-common)

;; Always resize mini-windows to fit their contents.
(setopt resize-mini-windows t)

;; TAB cycle if there are only few candidates
(setopt completion-cycle-threshold 2)

;; Hide commands in M-x which do not apply to the current mode.  Corfu commands
;; are hidden, since they are not used via M-x.  This setting is useful beyond
;; Corfu.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Don't let `completion-at-point' interfere with indentation.
(setopt tab-always-indent 'complete)

;; `completion-at-point' is often bound to M-TAB, but that conflicts with OS behavior.
;; We also want to preserve "C-S-SPC" , the Emacs default binding for `set-mark-command'.
(keymap-global-set "C-S-SPC" #'completion-at-point)
(after! orderless
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides '((file (styles partial-completion))
                                          (command (styles +orderless-with-initialism))
                                          (variable (styles +orderless-with-initialism))
                                          (symbol (styles +orderless-with-initialism))))

  (setopt orderless-matching-styles '(orderless-regexp))
  (setopt orderless-style-dispatchers (list ;; #'+orderless-first-initialism-dispatch
                                       ;; #'+orderless-flex-if-twiddle-dispatch
                                       ;; #'+orderless-not-if-bang-dispatch
                                       #'+orderless-consult-dispatch
                                       #'orderless-affix-dispatch)))
(require 'lib-common)

(package! corfu
  (declare-function global-corfu-mode "corfu")

  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.05)
  (setopt corfu-cycle t)
  (setopt corfu-preselect 'prompt)
  (setopt corfu-count 8)
  (setopt corfu-max-width 80)
  (setopt corfu-on-exact-match nil)
  (setopt corfu-scroll-margin 5)

  (setopt corfu-quit-at-boundary 'separator)
  (setopt corfu-quit-no-match 'separator)

  ;; Trigger insertion of the separator with "M-SPC".  The character will appear
  ;; to be inserted into the buffer.  Upon selecting a candidate (or aborting,
  ;; or whatver), the extra character will be removed.
  (setopt corfu-separator ?_)

  (global-corfu-mode))
(after! corfu
  (define-keymap :keymap corfu-map
    "TAB" #'corfu-next
    "S-TAB"  #'corfu-previous
    "RET" nil
    "C-h" #'corfu-info-documentation
    "C-n" nil
    "C-p" nil
    "C-RET" #'corfu-insert
    "M-." #'corfu-show-location
    "M-g" #'corfu-info-location ; default
    ;; "M-h" #'corfu-info-documentation    ; default
    "M-h" nil))
(after! meow
  (add-hook 'meow-insert-exit-hook #'corfu-quit))
(after! corfu
  (declare-function corfu-echo-mode "corfu-echo")
  (setopt corfu-echo-delay '(0.5 . 0.25))
  (corfu-echo-mode))
(after! corfu
  (declare-function corfu-popupinfo-mode "corfu-popupinfo")

  (setopt corfu-popupinfo-delay '(2.0 . 1.0))

  (corfu-popupinfo-mode)

  ;; Overrides previous binding to `corfu-info-documentation'.
  (keymap-set corfu-map "C-h" #'corfu-popupinfo-mode))
(after! corfu
  (declare-function corfu-history-mode "corfu-history")
  (corfu-history-mode))

;; Persist across sessions.
(after! (savehist corfu-history)
  (add-to-list 'savehist-additional-variables 'corfu-history))
(package! corfu-terminal)

(after! (corfu corfu-terminal)
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))
(package! kind-icon
  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-blend-background t)

  (require 'kind-icon)

  (after! corfu
    (defvar corfu-margin-formatters)
    (declare-function kind-icon-margin-formatter "kind-icon")

    (setopt kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))
(after! kind-icon
(after! kind-icon
  (declare-function kind-icon-reset-cache "kind-icon")
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'ceamx-after-enable-theme-hook #'kind-icon-reset-cache))
(after! dabbrev
  (setopt dabbrev-ignored-buffer-regexps
          (list
           ;; TODO: what does this pattern represent?
           ;;       why is it not same as eval result: (rx "` ")
           "\\` "
           (rx line-start " ")
           (rx (group (or (seq (opt (or "e" (seq "g" (opt "r")))) "tags")
                          "gpath"))
               (optional (group "<" (+ (any numeric)) ">")))))

  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)

  ;; Swap M-/ and C-M-/
  (keymap-global-set "M-/" #'dabbrev-completion)
  (keymap-global-set "C-M-/" #'dabbrev-expand))

;;;
(require 'lib-common)

(package! cape
  (setopt cape-dabbrev-check-other-buffers t)

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; "Character Mnemonics & Character Sets": <https://datatracker.ietf.org/doc/html/rfc1345>
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)

  (global-keys!
    "M-p p" #'completion-at-point ;; capf
    "M-p t" #'complete-tag        ;; etags
    "M-p d" #'cape-dabbrev        ;; or dabbrev-completion
    "M-p h" #'cape-history
    "M-p f" #'cape-file
    "M-p k" #'cape-keyword
    "M-p s" #'cape-elisp-symbol
    "M-p e" #'cape-elisp-block
    "M-p a" #'cape-abbrev
    "M-p l" #'cape-line
    "M-p w" #'cape-dict
    "M-p :" #'cape-emoji
    "M-p &" #'cape-sgml
    ;; ref: <https://datatracker.ietf.org/doc/html/rfc1345>
    "M-p r" #'cape-rfc1345))
(after! comint
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive))

(after! eglot
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

(after! lsp-mode
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

(provide 'init-completion)
;;; init-completion.el ends here
