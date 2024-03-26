;;; init-completion.el --- Completion interfaces -*- lexical-binding: t; -*-

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

;;  Configuration for completion-at-point.

;; TODO: Doom Emacs now has official Corfu module, check it out

;; FIXME: evil/meow escape does not quit completion
;; <https://github.com/emacs-evil/evil-collection/issues/676>

;; - <https://github.com/minad/corfu>
;; - <https://github.com/minad/cape>
;; - <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>

;;; Code:

(require 'lib-completion)
(require 'lib-common)

;; Always resize mini-windows to fit their contents.
(setopt resize-mini-windows t)

;; TAB cycle if there are only few candidates
(setopt completion-cycle-threshold 3)

;; Hide commands in M-x which do not apply to the current mode.  Corfu commands
;; are hidden, since they are not used via M-x.  This setting is useful beyond
;; Corfu.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Don't let `completion-at-point' interfere with indentation.
(setopt tab-always-indent t)

;; `completion-at-point' is often bound to M-TAB, but that conflicts with OS behavior.
;; We also want to preserve "C-S-SPC" , the Emacs default binding for `set-mark-command'.
(keymap-global-set "C-S-SPC" #'completion-at-point)

;;; ~corfu~: COmpletion in Region FUnction

(package! corfu
  (declare-function global-corfu-mode "corfu")

  (setopt corfu-auto t)
  (setopt corfu-cycle t)
  (setopt corfu-quit-at-boundary t)
  (setopt corfu-quit-no-match 'separator)
  (setopt corfu-separator ?_)
  (setopt corfu-preselect 'prompt)

  ;; (setopt corfu-on-exact-match nil)
  ;; (setopt corfu-scroll-margin 5)
  (setopt corfu-auto-delay 0.1)
  ;; TODO: maybe enable when invoked manually?
  (setopt corfu-preview-current nil)

  (global-corfu-mode)

  ;; Default values, unless otherwise stated.
  (define-keymap :keymap corfu-map
    "M-g" #'corfu-info-location
    "M-h" #'corfu-info-documentation))

;;;; ~corfu-echo-mode~: Show candidate docs in echo area

;; Probably redundant when ~corfu-popupinfo-mode~ is in use.

(after! corfu
  (declare-function corfu-echo-mode "corfu-echo")
  (setopt corfu-echo-delay '(0.5 . 0.25))
  (corfu-echo-mode))

;;;; ~corfu-popupinfo~: Display candidate docs or location in popup

(after! corfu
  (declare-function corfu-popupinfo-mode "corfu-popupinfo")
  (setopt corfu-popupinfo-delay '(1.0 . 0.5))
  (corfu-popupinfo-mode))

;;;; ~corfu-history-mode~: Sort candidates by history position

(after! corfu
  (declare-function corfu-history-mode "corfu-history")
  (corfu-history-mode))

;; Persist across sessions.
(after! (savehist corfu-history)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;;; ~corfu-terminal~: Terminal support for Corfu

;; <https://codeberg.org/akib/emacs-corfu-terminal>

;;  Corfu-endorsed solution to making it usable in terminal.
;;  See also `popon', the utility library powering the interface.

(package! corfu-terminal)

(after! (corfu corfu-terminal)
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

;;; ~corfu-doc-terminal~: Support doc flyouts in terminal

;; <https://codeberg.org/akib/emacs-corfu-doc-terminal>

;;  Support for completion candidate documentation flyouts in terminal.

;;  FIXME: missing `corfu-doc' dependency -- that package was integrated into
;;  corfu core, but still not available. since this is a non-essential
;;  enhancement, it will probably be removed.
;;
;; (use-package corfu-doc-terminal
;;   ;; FIXME: :elpaca (corfu-doc-terminal :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
;;   :after (corfu-terminal)
;;   :unless (display-graphic-p)
;;   :config
;;   (corfu-doc-terminal-mode +1))

;;; ~kind-icon~: Add icons to Corfu candidates

;; <https://github.com/jdtsmith/kind-icon>

;; Colorful icons for completion-at-point interfaces

(package! kind-icon
  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-blend-background t)
  (setopt kind-icon-default-face 'corfu-default)

  (after! (svg-lib corfu)
    (require 'kind-icon)))

(after! kind-icon
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'after-enable-theme-hook #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Configure ~dabbrev~

(after! dabbrev
  ;; TODO: what does this pattern represent?
  ;;       why is it not same as eval result: (rx "` ")
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)

  ;; Swap M-/ and C-M-/
  (keymap-global-set "M-/" #'dabbrev-completion)
  (keymap-global-set "C-M-/" #'dabbrev-expand))

;;; ~cape~: Completion-At-Point Extensions

;; <https://github.com/minad/cape/blob/main/README.org>

(package! cape
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

(provide 'init-completion)
;;; init-completion.el ends here
