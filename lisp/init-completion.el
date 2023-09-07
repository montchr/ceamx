;;; init-completion.el --- Completion interfaces -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

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

;; - <https://github.com/minad/corfu>
;; - <https://github.com/minad/cape>
;; - <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>

;;; Code:

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(use-package corfu
  :elpaca (corfu :host github :repo "minad/corfu"
                 :files (:defaults "extensions/*"))

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode))

  :config
  ;; (setq! corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (setq! corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (setq! corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (setq! corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (setq! corfu-scroll-margin 5)        ;; Use scroll margin
  (setq! corfu-cycle t)
  (setq! corfu-auto t)
  (setq! corfu-auto-delay 0.2)
  (setq! corfu-separator ?\s)
  (setq! corfu-preview-current nil))

(use-package dabbrev :elpaca nil
  :bind
  ;; Swap M-/ and C-M-/
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))

  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  ;; Bind dedicated completion commands
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-elisp-symbol)
         ("M-p a" . cape-abbrev)
         ;; FIXME: obsolete
         ("M-p i" . cape-ispell)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ;; ref: <https://datatracker.ietf.org/doc/html/rfc1345>
         ("M-p r" . cape-rfc1345))

  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; ref: <https://datatracker.ietf.org/doc/html/rfc1345>
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(provide 'init-completion)
;;; init-completion.el ends here
