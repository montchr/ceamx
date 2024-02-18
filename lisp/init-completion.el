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

;; TODO: Some interesting maybe-useful stuff in here: <https://github.com/doomemacs/doomemacs/pull/7002/files>

;; - <https://github.com/minad/corfu>
;; - <https://github.com/minad/cape>
;; - <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>

;;; Code:

(require 'lib-completion)
(require 'lib-common)

;; TAB cycle if there are only few candidates
(setopt completion-cycle-threshold 3)

;; Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Don't let `completion-at-point' interfere with indentation.
(setopt tab-always-indent t)

;; `completion-at-point' is often bound to M-TAB, but that conflicts with OS behavior.
;; We also want to preserve "C-S-SPC" , the Emacs default binding for `set-mark-command'.
(keymap-global-set "C-S-SPC" #'completion-at-point)

(after! [evil]
  ;; Since we now know `evil' is loaded, it's reasonable to overwrite the mark binding.
  (keymap-set evil-insert-state-map "C-SPC" #'completion-at-point)
  ;; But we don't want to lose mark capabilities entirely.
  (keymap-set evil-insert-state-map "C-S-SPC" #'set-mark-command))

;; FIXME: evil escape does not quit completion when `evil-disable-insert-state-bindings' is t
;; <https://github.com/emacs-evil/evil-collection/issues/676>
(use-package corfu
  ;; FIXME: :elpaca (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode))

  :config
  ;; Stay out of my way!
  (setopt corfu-quit-at-boundary t)
  (setopt corfu-quit-no-match 'separator)
  ;; (setopt corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (setopt corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (setopt corfu-scroll-margin 5)        ;; Use scroll margin
  (setopt corfu-cycle t)
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.2)
  (setopt corfu-separator ?\s)
  ;; TODO: maybe enable when invoked manually?
  (setopt corfu-preview-current nil))

;;; `corfu-terminal' :: <https://codeberg.org/akib/emacs-corfu-terminal>
;;  Corfu-endorsed solution to making it usable in terminal.
;;  See also `popon', the utility library powering the interface.
(use-package corfu-terminal
  ;; FIXME: :elpaca (corfu-terminal :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless (display-graphic-p)
  :after (popon corfu)
  :config
  (corfu-terminal-mode +1))

;;; `corfu-doc-terminal' :: <https://codeberg.org/akib/emacs-corfu-doc-terminal>
;;  Support for completion candidate documentation flyouts in terminal.
;;
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

;;; kind-icon :: <https://github.com/jdtsmith/kind-icon>

;; Colorful icons for completion-at-point interfaces

(use-package kind-icon
  :demand t
  :after (svg-lib corfu)
  :commands (kind-icon-reset-cache)
  :autoload (kind-icon-margin-formatter)

  :init
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'after-enable-theme-hook #'kind-icon-reset-cache)

  :config
  (defvar corfu-margin-formatters)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-blend-background t)
  (setopt kind-icon-default-face 'corfu-default))

(use-feature! dabbrev
  :config
  ;; Swap M-/ and C-M-/
  (keymap-global-set "M-/"    #'dabbrev-completion)
  (keymap-global-set "C-M-/"  #'dabbrev-expand)

  ;; TODO: look into using `rx' for easier building of regexps
  (setopt dabbrev-ignored-buffer-regexps
          '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; ref: <https://datatracker.ietf.org/doc/html/rfc1345>
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)

  :config
  ;; FIXME: bind to a real prefix map
  (keymap-global-set "M-p p" #'completion-at-point) ;; capf
  (keymap-global-set "M-p t" #'complete-tag)        ;; etags
  (keymap-global-set "M-p d" #'cape-dabbrev)        ;; or dabbrev-completion
  (keymap-global-set "M-p h" #'cape-history)
  (keymap-global-set "M-p f" #'cape-file)
  (keymap-global-set "M-p k" #'cape-keyword)
  (keymap-global-set "M-p s" #'cape-elisp-symbol)
  (keymap-global-set "M-p a" #'cape-abbrev)
  (keymap-global-set "M-p l" #'cape-line)
  (keymap-global-set "M-p w" #'cape-dict)
  ;; ref: <https://datatracker.ietf.org/doc/html/rfc1345>
  (keymap-global-set "M-p r" #'cape-rfc1345))

(provide 'init-completion)
;;; init-completion.el ends here
