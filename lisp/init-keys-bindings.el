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

(require 'ceamx-paths)

(require 'config-help)
(require 'config-keys)

(require 'lib-common)
(require 'lib-keys)


;;; Mouse/trackpad support

;;;; Horizontal scroll support (without scrollbars)

(keymap-global-set "<wheel-left>" #'scroll-left)
(keymap-global-set "<wheel-right>" #'scroll-right)

;;; Navigation maps

;; TODO: merge both `ceamx-go-prev-map' and `ceamx-go-next-map' into the
;;       appropriate leaderless evil motion state bindings e.g. "[" and "]" in
;;       normal state. i tried doing that quickly but am not sure how to
;;       determine what is bound to "[" or "]" directly. i would have thought
;;       prefix mapped to keymap, but maybe evil is weirder than that...

;;;; Previous

;; (def-arm! ceamx-go-prev-map "[" "[Prev]"
;;   "TAB" #'tab-previous
;;   "["   #'previous-buffer
;;   "b"   #'previous-buffer
;;   "e"   #'flycheck-previous-error
;;   "F"   #'previous-window-any-frame
;;   "t"   #'tab-previous)

;;;; Next

;; (def-arm! ceamx-go-next-map "]" "[Next]"
;;   "TAB" #'tab-next
;;   "]"   #'next-buffer
;;   "b"   #'next-buffer
;;   "e"   #'flycheck-next-error
;;   "F"   #'next-window-any-frame
;;   "t"   #'tab-next)

;;;; Goto

;; TODO: make this more convenient

;; (def-map! ceamx-goto-map
;;   "d" '("definition" . xref-find-definitions)
;;   "r" '("references" . xref-find-references))

;;; "B" => Bookmarks

;; (def-arm! ceamx-bookmark-map "B" "[Bookmarks]"
;;   "F" #'burly-bookmark-frames
;;   "W" #'burly-bookmark-windows)

;;; "b" => Buffers

;; (def-arm! ceamx-buffer-map "b" "[Buffer]"
;;   ;; FIXME: only consider file-visiting buffers, or perhaps buffers i am editing (recent)
;;   "[" '("prev" . previous-buffer)
;;   "]" '("next" . next-buffer)

;;   "b" `("switch..." . consult-project-buffer)
;;   "B" '("switch (any)..." . consult-buffer)
;;   "d" '("close" . kill-current-buffer)
;;   "k" '("close (+win)" . kill-this-buffer)
;;   "M" '("*Messages*" . view-echo-area-messages)
;;   "o" '("other" . mode-line-other-buffer)
;;   "r" '("revert" . revert-buffer)
;;   "R" '("rename..." . rename-buffer)
;;   "s" '("save" . save-buffer)
;;   "S" '("save all..." . save-some-buffers)
;;   ;; TODO: maybe find a better place for this binding
;;   "u" '("visual undo..." . vundo)
;;   "x" '("*scratch*" . scratch-buffer)
;;   "X" `("*scratch* (m)" . ,(cmd! (scratch major-mode))))

;;; "c" => Code

;; (def-arm! ceamx-code-map "C" "Code"
;;   "a" '("action.." . eglot-code-actions)
;;   "d" #'xref-find-definitions
;;   "h" #'helpful-at-point
;;   "i" #'iedit-mode
;;   "r" '("rename..." . eglot-rename))

;;; "e" => Eval

;; (def-arm! ceamx-eval-map "e" "[Eval]"
;;   "b" #'eval-buffer
;;   "d" #'eval-defun
;;   "e" #'eval-last-sexp
;;   "E" #'eval-expression
;;   "i" #'ielm
;;   "r" #'eval-region)

;;; "f" => Files

(def-arm! ceamx-file-map "f" "[File]"
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name

  "C" '("copy..." . ceamx/copy-this-file)
  "d" '("diff with..." . ceamx/diff-with-file)

  ;; FIXME: kill buffer on file deletion
  "D" '("delete" . ceamx/delete-this-file)

  ;; TODO: show dirvish preview instead of dired preview
  "f" '("find (g)..." . find-file)

  "R" '("rename/move..." . ceamx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file))

;;; "F" => Frames

;; (def-arm! ceamx-frame-map "F" "[Frame]"
;;   "b" '("save layout..." . burly-bookmark-frames)
;; 	"F" '("switch to..." . select-frame-by-name)
;;   "n" '("create" . make-frame-on-current-monitor)
;;   "N" '("create on monitor..." . make-frame-on-monitor)
;;   "o" '("other" . other-frame)
;;   "R" '("rename..." . set-frame-name)
;;   "[" '("prev" . previous-window-any-frame)
;;   "]" '("next" . next-window-any-frame))

;;; "g" => Git

;; TODO: disabled to try out vanilla keybinds with meow keypad defaults
;; (def-arm! ceamx-git-map "g" "[Git]"
;;   "b" #'magit-branch
;;   "B" #'magit-blame
;;   "f" #'magit-find-file
;;   "g" #'magit-status
;;   "G" #'magit-dispatch
;;   "l" #'magit-log-buffer-file
;;   "s" #'magit-stage-file
;;   "S" #'magit-unstage-file
;;   "t" #'git-timemachine)

;;; Help -- "C-h"

(define-keymap :keymap help-map
  "c" #'helpful-callable
  "C" #'helpful-command
  "f" #'helpful-function
  "F" #'describe-face
  "h" #'helpful-at-point
  ;; TODO: add as command
  ;; FIXME: create directory if not exists
  ;; "H" `("cheatsheet..." . ,(cmd!!
  ;;                            #'ido-find-file-in-dir
  ;;                            current-prefix-arg
  ;;                            ceamx-cheatsheets-dir))
  ;; TODO:      example:   (cl-find-if #'fboundp '(harper-dad-joint helpful-at-point describe-key))
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

;;; "i" => Insertions

;; (def-arm! ceamx-insert-map "i" "[Insert]"
;;   "t"  #'tempel-insert
;;   "y"  #'yank-from-kill-ring)

;;; Notes / Org-Mode

;;;; "X" / "n o c" => Org-Capture

;; (def-arm! ceamx-capture-map "X" "[Capture]"
;;   "X" '("capture..." . org-capture))

;;;; "o" / "n o" => Org-Mode

;; (def-arm! ceamx-org-map "o" "[Org-Mode]"
;;   "a" #'org-agenda
;;   "c" '("capture..." . ceamx-capture-map)
;;   "l" #'org-store-link
;;   "t" '("todos" . org-todo-list))

;;;; "n" => Notes

;; (def-arm! ceamx-notes-map "n" "[Note]"
;;   "b" #'denote-backlinks
;;   "c" #'org-capture
;;   "d" #'denote-date
;;   "f" '("[find]" . (keymap))
;;   "f f" #'denote-find-link
;;   "f b" #'denote-find-backlink
;;   "i" #'denote-link                     ; "insert" mnemonic
;;   "I" #'denote-add-links
;;   "j" #'my-denote-journal               ; our custom command
;;   "n" #'denote
;;   "N" #'denote-type
;;   "o" '("[Org-Mode]" . ceamx-org-map)
;;   ;; Note that `denote-rename-file' can work from any context, not just
;;   ;; Dired buffers.  That is why we bind it here to the `global-map'.
;;   "r" #'denote-rename-file
;;   "R" #'denote-rename-file-using-front-matter
;;   "s" #'denote-subdirectory
;;   "t" #'denote-template
;;   ;; "zettelkasten" mnemonic
;;   "z" #'denote-signature)

;;; "O" => Open

;; (def-arm! ceamx-open-map "o" "[Open]"
;;   "d" #'dired
;;   "e" #'eshell
;;   "l" '("link-at-point" . link-hint-open-link-at-point)
;;   "L" '("link..." . link-hint-open-link)
;;   "m" '("mail" . compose-mail)
;;   "n" '("news" . newsticker-show-news)
;;   "s" #'suggest)

;;; "p" => Projects

;; (def-arm! ceamx-project-map "p" "[Project]"
;;   "a" '("add..." . projectile-add-known-project)
;;   "f" '("find file..." . projectile-find-file)
;;   "i" '("invalidate cache" . projectile-invalidate-cache)
;;   "p" '("switch..." . projectile-switch-project))

;;; "q" => Session

;;;; "q p" Package Management

;; (def-map! ceamx-packages-map
;;   "b" #'embark-browse-package-url
;;   "c" #'package-autoremove
;;   "d" #'package-delete
;;   "i" #'describe-package
;;   "I" #'package-install
;;   "p" #'list-packages
;;   "r" #'package-refresh-contents
;;   "s" #'use-package-report
;;   "u" #'package-upgrade
;;   "U" #'package-upgrade-all)


;; (def-arm! ceamx-session-map "q" "[Session]"
;;   "f" '("font..." . fontaine-set-preset)
;;   "p" '("packages" . ceamx-packages-map)
;;   "q" '("close frame" . delete-frame)
;;   "Q" '("save+quit" . save-buffers-kill-emacs)
;;   "r" '("restart" . restart-emacs)
;;   "t" '("theme..." . consult-theme))

;;; "s" => Search

(def-arm! ceamx-search-map "s" "[Search]"
  "d" `("directory..." . ,(cmd! (consult-ripgrep
                                  (file-name-directory buffer-file-name))))
  "h" '("history..." . consult-isearch-history)
  "j" '("symbols (f)..." . consult-lsp-file-symbols)
  "J" '("symbols (g)..." . consult-lsp-symbols)
  "l" '("library..." . (lambda () (interactive "P")
                         (call-interactively
                           (if %
                             #'find-library-other-window
                             #'find-library))))
  "o" '("outline (f)..." . consult-outline)
  ;; TODO: use thing-at-point as default value like `projectile-ripgrep' (which cannot find ripgrep)

  "p" '("grep (p)..." . consult-ripgrep)
  "R" #'project-query-replace-regexp
  ;; "R" '("replace (p)..." . projectile-replace)
  "s" '("line (f)..." . consult-line)
  "v" '("variable" . find-variable-at-point)
  "V" '("variable..." . find-variable)
  ;; "x" '("refs (p)" . projectile-find-references)
  )

;;; "t" => Toggles

;; (def-arm! ceamx-toggle-map "t" "[Toggle]"
;;   "l" #'display-line-numbers-mode
;;   "L" #'line-number-mode
;;   "f" #'flycheck-mode
;;   "t" #'treemacs
;;   "w" '("side windows" . window-toggle-side-windows))

;;; "w" => Window

(use-feature! init-window
  :commands (ceamx/transient-window)
  :config
  (keymap-global-set "C-x w" #'ceamx/transient-window))

(define-keymap :keymap (current-global-map)
  "C-x o" #'ace-window
  "C-x =" #'balance-windows             ; prev: C-x +
  "C-x +" nil                           ; TODO: replace
  )

;;; "y" => Copy / Evil Yank

;; (def-arm! ceamx-yank-map "y" "[Yank]"
;;   "l" '("link (visible)" . link-hint-copy-link))

;;; "TAB" => Tabs

;; (def-arm! ceamx-tab-map "TAB" "[Tab]"
;;   "TAB"  '("other" . tab-recent)
;;   "d"    '("delete" . tab-close)
;;   "h"    '("prev" . tab-previous)
;;   "l"    '("next" . tab-next)
;;   "n"    '("new" . tab-new)
;;   "t"    '("other" . tab-recent)
;;   "x"    '("close" . tab-close))

;;; Top-level leader map

;; TODO: why not `other-buffer'?
(leader-key! "`"   '("other buffer" . mode-line-other-buffer))
;; (leader-key! "j"   '("jump: line..." . consult-line))

;; (leader-key! "a" '("agenda..." . consult-org-agenda))

;; TODO: install
;; (leader-key! "?"   #'consult-apropos)

;; (define-keymap :keymap mode-specific-map
;;   ;; TODO: "a" => Agenda (in progress)
;;   ;; TODO: "d"
;;   ;; "k"
;;   ;; "l"
;;   ;; "m" => RESERVED for mode-specific local maps
;;   ;; "r"
;;   ;; "u"
;;   ;; "v"
;;   ;; "z"
;;   )

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
  "C-:" #'avy-goto-char
  "C-'" #'avy-goto-char-2
  "C-." #'avy-resume
  "M-j" #'avy-goto-char-timer
  ;; FIXME: does not trigger autoload from use-package
  "C-x u" #'vundo
  ;; TODO: find a better place
  ;; "C-x SPC" #'hydra-rectangle/body
  )

(provide 'init-keys-bindings)
;;; init-keys-bindings.el ends here
