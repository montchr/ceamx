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

;; TODO: quikgrok descriptions for `cmx-window-map' defs
;; TODO: vim-like case invert! <https://gitlab.com/slotThe/dotfiles/-/blob/77393d030021a3524c03f22bbb4a4ca75965a9fd/emacs/.config/emacs/lisp/keybindings.el#L79-92>

;;; Code:

(require 'ceamx-paths)

(require 'config-help)
(require 'config-keys)

(require 'lib-common)
(require 'lib-keys)

;;
;;; Mouse/trackpad support
;;

;;; Horizontal scroll support (without scrollbars).
(keymap-global-set "<wheel-left>" #'scroll-left)
(keymap-global-set "<wheel-right>" #'scroll-right)


;;
;;; Leader maps
;;

;;; Navigation maps

;; TODO: merge both `cmx-go-prev-map' and `cmx-go-next-map' into the
;;       appropriate leaderless evil motion state bindings e.g. "[" and "]" in
;;       normal state. i tried doing that quickly but am not sure how to
;;       determine what is bound to "[" or "]" directly. i would have thought
;;       prefix mapped to keymap, but maybe evil is weirder than that...

;;;; Previous

(def-arm! cmx-go-prev-map "[" "[Prev]"
  "TAB" #'tab-previous
  "["   #'previous-buffer
  "b"   #'previous-buffer
  "e"   #'flycheck-previous-error
  "F"   #'previous-window-any-frame
  "t"   #'tab-previous)

;;;; Next

(def-arm! cmx-go-next-map "]" "[Next]"
  "TAB" #'tab-next
  "]"   #'next-buffer
  "b"   #'next-buffer
  "e"   #'flycheck-next-error
  "F"   #'next-window-any-frame
  "t"   #'tab-next)

;; TODO: make this more convenient
;;;; Goto
(def-map! cmx-goto-map
  "d" '("definition" . xref-find-definitions)
  "r" '("references" . xref-find-references))

;;
;;; "B" => Bookmarks
;;

(def-arm! cmx-bookmark-map "B" "[Bookmarks]"
  "F" #'burly-bookmark-frames
  "W" #'burly-bookmark-windows)

;;
;;; "b" => Buffers
;;

(def-arm! cmx-buffer-map "b" "[Buffer]"
  ;; FIXME: only consider file-visiting buffers, or perhaps buffers i am editing (recent)
  "[" '("prev" . previous-buffer)
  "]" '("next" . next-buffer)

  "b" `("switch..." . consult-project-buffer)
  "B" '("switch (any)..." . consult-buffer)
  "d" '("close" . kill-current-buffer)
  "k" '("close (+win)" . kill-this-buffer)
  "M" '("*Messages*" . view-echo-area-messages)
  "o" '("other" . mode-line-other-buffer)
  "r" '("revert" . revert-buffer)
  "R" '("rename..." . rename-buffer)
  "s" '("save" . save-buffer)
  "S" '("save all..." . save-some-buffers)
  ;; TODO: maybe find a better place for this binding
  "u" '("visual undo..." . vundo)
  "x" '("*scratch*" . scratch-buffer)
  "X" `("*scratch* (m)" . ,(cmd! (scratch major-mode))))

;;
;;; "c" => Code
;;

(def-arm! cmx-code-map "C" "Code"
  "a" '("action.." . eglot-code-actions)
  "d" #'xref-find-definitions
  "h" #'helpful-at-point
  "i" #'iedit-mode
  "r" '("rename..." . eglot-rename))

;;
;;; "e" => Eval
;;

(def-arm! cmx-eval-map "e" "[Eval]"
  "b" #'eval-buffer
  "d" #'eval-defun
  "e" #'eval-last-sexp
  "E" #'eval-expression
  "i" #'ielm
  "r" #'eval-region)

;;
;;; "f" => Files
;;

(def-arm! cmx-file-map "f" "[File]"
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name
  "C" '("copy..." . cmx/copy-this-file)
  "d" '("diff with..." . cmx/diff-with-file)
  ;; FIXME: kill buffer on file deletion
  "D" '("delete" . cmx/delete-this-file)
  ;; TODO: show dirvish preview instead of dired preview
  "f" '("find (g)..." . find-file)
  "R" '("rename/move..." . cmx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file))

;;
;;; "F" => Frames
;;

(def-arm! cmx-frame-map "F" "[Frame]"
  "b" '("save layout..." . burly-bookmark-frames)
	"F" '("switch to..." . select-frame-by-name)
  "n" '("create" . make-frame-on-current-monitor)
  "N" '("create on monitor..." . make-frame-on-monitor)
  "o" '("other" . other-frame)
  "R" '("rename..." . set-frame-name)
  "[" '("prev" . previous-window-any-frame)
  "]" '("next" . next-window-any-frame))

;;
;;; "g" => Git
;;

;; TODO: disabled to try out vanilla keybinds with meow keypad defaults
;; (def-arm! cmx-git-map "g" "[Git]"
;;   "b" #'magit-branch
;;   "B" #'magit-blame
;;   "f" #'magit-find-file
;;   "g" #'magit-status
;;   "G" #'magit-dispatch
;;   "l" #'magit-log-buffer-file
;;   "s" #'magit-stage-file
;;   "S" #'magit-unstage-file
;;   "t" #'git-timemachine)

;;
;;; "h" => Help
;;

;; NOTE: This only modifies the existing `help-map' bound to C-h.

(define-keymap :keymap help-map
  "c" #'helpful-command
  "f" #'helpful-function
  "F" #'describe-face
  "h" #'helpful-at-point
  ;; TODO: add as command
  ;; FIXME: create directory if not exists
  "H" `("cheatsheet..." . ,(cmd!!
                             #'ido-find-file-in-dir
                             current-prefix-arg
                             cmx-cheatsheets-dir))
  ;; NOTE: currently `meow-describe-key'
  ;; TODO: move corresponding meow binding here with fallback to default
  ;;       example:   (cl-find-if #'fboundp '(harper-dad-joint helpful-at-point describe-key))
  ;; "k" #'describe-key
  "K" #'describe-key-briefly
  "l" #'find-library
  "o" #'helpful-symbol
  "s" #'suggest
  "t" `("text-props (pt)" . ,(cmd!!
                               #'describe-text-properties
                               current-prefix-arg
                               (point)))
  ;; FIXME: `helpful' is not very helpful when it errors out on so many occasions
  ;;        <https://github.com/Wilfred/helpful/issues/329>
  "v" #'helpful-variable

  ;; Parity with the corresponding unmodded keys.
  ;; Primarily for Meow keypad, but also sometimes feels more natural to keep
  ;; holding Ctrl anyway.
  "C-h" #'helpful-at-point
  "C-o" #'helpful-symbol)


;;
;;; "i" => Insertions
;;

(def-arm! cmx-insert-map "i" "[Insert]"
  "t"  #'tempel-insert
  "y"  #'yank-from-kill-ring)

;;
;;; Notes / Org-Mode
;;

;;;; "X" / "n o c" => Org-Capture
(def-arm! cmx-capture-map "X" "[Capture]"
  "X" '("capture..." . org-capture))

;;;; "o" / "n o" => Org-Mode
(def-arm! cmx-org-map "o" "[Org-Mode]"
  "a" #'org-agenda
  "c" '("capture..." . cmx-capture-map)
  "l" #'org-store-link
  "t" '("todos" . org-todo-list))

;;;; "n" => Notes
(def-arm! cmx-notes-map "n" "[Note]"
  "b" #'denote-backlinks
  "c" #'org-capture
  "d" #'denote-date
  "f" '("[find]" . (keymap))
  "f f" #'denote-find-link
  "f b" #'denote-find-backlink
  "i" #'denote-link                     ; "insert" mnemonic
  "I" #'denote-add-links
  "j" #'my-denote-journal               ; our custom command
  "n" #'denote
  "N" #'denote-type
  "o" '("[Org-Mode]" . cmx-org-map)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired buffers.  That is why we bind it here to the `global-map'.
  "r" #'denote-rename-file
  "R" #'denote-rename-file-using-front-matter
  "s" #'denote-subdirectory
  "t" #'denote-template
  ;; "zettelkasten" mnemonic
  "z" #'denote-signature)

;;
;;; "O" => Open
;;

(def-arm! cmx-open-map "o" "[Open]"
  "d" #'dired
  "e" #'eshell
  "l" '("link-at-point" . link-hint-open-link-at-point)
  "L" '("link..." . link-hint-open-link)
  "m" '("mail" . compose-mail)
  "n" '("news" . newsticker-show-news)
  "s" #'suggest)

;;
;;; "p" => Projects
;;

(def-arm! cmx-project-map "p" "[Project]"
  "a" '("add..." . projectile-add-known-project)
  "f" '("find file..." . projectile-find-file)
  "i" '("invalidate cache" . projectile-invalidate-cache)
  "p" '("switch..." . projectile-switch-project))

;;
;;; "q" => Session
;;

;;;; "q p" Package Management
(def-map! cmx-packages-map
  "c" #'package-autoremove
  "d" #'package-delete
  "i" #'describe-package
  "I" #'package-install
  "p" #'list-packages
  "r" #'package-refresh-contents
  "s" #'use-package-report
  "u" #'package-upgrade
  "U" #'package-upgrade-all)


(def-arm! cmx-session-map "q" "[Session]"
  "f" '("font..." . fontaine-set-preset)
  "p" '("packages" . cmx-packages-map)
  "q" '("close frame" . delete-frame)
  "Q" '("save+quit" . save-buffers-kill-emacs)
  "r" '("restart" . restart-emacs)
  "t" '("theme..." . consult-theme))

;;
;;; "s" => Search
;;

(def-arm! cmx-search-map "s" "[Search]"
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
  "R" '("replace (p)..." . projectile-replace)
  "s" '("line (f)..." . consult-line)
  "v" '("variable" . find-variable-at-point)
  "V" '("variable..." . find-variable)
  "x" '("refs (p)" . projectile-find-references))

;;
;;; "t" => Toggles
;;

(def-arm! cmx-toggle-map "t" "[Toggle]"
  "l" #'display-line-numbers-mode
  "L" #'line-number-mode
  "f" #'flycheck-mode
  "t" #'treemacs)

;;
;;; "w" => Window
;;

;; Largely based on Doom bindings, which are based on `evil-window-map'.

;; FIXME: assumes `evil'
(def-arm! cmx-window-map "w" "[Window]"
  ;;; default
  "w" #'ace-window

  ;;; tune
  "="  #'balance-windows
  ;; ">"  #'evil-window-increase-width
  ;; "<"  #'evil-window-decrease-width

  ;;; navigate
  ;; "h"      #'evil-window-left
  ;; "j"      #'evil-window-down
  ;; "k"      #'evil-window-up
  ;; "l"      #'evil-window-right
  ;; "C-h"    #'evil-window-left
  ;; "C-j"    #'evil-window-down
  ;; "C-k"    #'evil-window-up
  ;; "C-l"    #'evil-window-right
  "C-w"    #'other-window

  ;;; swap
  ;; "H"      #'cmx/evil/window-move-left
  ;; "J"      #'cmx/evil/window-move-down
  ;; "K"      #'cmx/evil/window-move-up
  ;; "L"      #'cmx/evil/window-move-right
  ;; "r"      #'evil-window-rotate-downwards
  ;; "R"      #'evil-window-rotate-upwards
  "C-S-w"  #'ace-swap-window

  ;;; mutate
  ;; "d"      #'evil-window-delete
  ;; "n"      #'evil-window-new
  "o"      #'delete-other-windows
  ;; "s"      #'evil-window-split
  "T"      #'tear-off-window
  "u"      #'winner-undo
  ;; "v"      #'evil-window-vsplit
  "C-c"    #'ace-delete-window
  "C-r"    #'winner-redo
  "C-u"    #'winner-undo)

;;
;;; "y" => Copy / Evil Yank
;;

(def-arm! cmx-yank-map "y" "[Yank]"
  "l" '("link (visible)" . link-hint-copy-link))

;;
;;; "TAB" => Tabs
;;

(def-arm! cmx-tab-map "TAB" "[Tab]"
  "TAB"  '("other" . tab-recent)
  "d"    '("delete" . tab-close)
  "h"    '("prev" . tab-previous)
  "l"    '("next" . tab-next)
  "n"    '("new" . tab-new)
  "t"    '("other" . tab-recent)
  "x"    '("close" . tab-close))

;;
;;; Top-level leader map
;;

;; TODO: why not `other-buffer'?
(leader-key! "`"   '("other buffer" . mode-line-other-buffer))
;; FIXME: conflicts with meow keypad
;; (leader-key! "SPC" '("project buffer..." . consult-project-buffer))
(leader-key! "j"   '("jump: line..." . consult-line))

;; via <https://github.com/mclear-tools/dotemacs/blob/dc18ceebe9b3580b6b4deeb033f282670cb4df8b/cpm-setup-meow.el>
(leader-key! ")" '("slurp ->" . "C-)"))
(leader-key! "(" '("<- slurp" . "C-("))
(leader-key! "}" "C-}")
(leader-key! "." "M-.")
(leader-key! ";" #'comment-line)
(leader-key! "j" #'avy-goto-char-2)

(leader-key! "a" '("agenda..." . consult-org-agenda))

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

(keymap-global-set cmx-leader-alt-key 'mode-specific-command-prefix)
(keymap-global-set "<f12>" 'mode-specific-command-prefix)

(after! [evil]
  ;; Bind leader key to existing leader map.
  (evil-define-key* '(normal visual motion) 'global
    (kbd cmx-leader-key) 'mode-specific-command-prefix)
  (after! [magit]
    (keymap-set magit-mode-map
      cmx-leader-key #'mode-specific-command-prefix))

  ;; Bind leader to `,' (comma).
  (evil-define-key* '(normal visual motion) 'global
    "," 'mode-specific-command-prefix))

;;
;;; Global Bindings
;;

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
  "C-x u" #'vundo
  ;; TODO: find a better place
  ;; "C-x SPC" #'hydra-rectangle/body
  )


;;
;;; Evil Bindings
;;

(after! [evil]
  ;; Don't let evil trample over the immensely-useful question mark.
  (evil-define-key '(normal motion) 'global "?" nil)

  ;;; Normal state
  (define-keymap :keymap evil-normal-state-map
    "f"    #'evil-avy-goto-char-in-line
    "F"    #'avy-goto-char-timer
    "g d"  #'xref-find-definitions
    "g D"  #'xref-find-references
    "g s"  #'avy-goto-char-timer
    "K"    #'helpful-at-point)

  ;;; Insert state
  ;;
  ;; Continuing from `init-keys-evil', where we disabled all insert
  ;; state bindings to allow for default Emacs bindings to remain, now
  ;; we can add some of the missing `evil' bindings back selectively.
  ;;
  ;; See <https://github.com/noctuid/evil-guide?tab=readme-ov-file#switching-between-evil-and-emacs>
  ;; however it is not guaranteed to be accurate.
  (define-keymap :keymap evil-insert-state-map
    "C-@" #'evil-paste-last-insertion-and-stop-insert
    "C-g" #'evil-normal-state ; custom addition
    "C-o" #'evil-execute-in-normal-state
    "C-q" #'evil-quoted-insert
    "C-r" #'evil-paste-from-register)

  ;;; Motion state
  (define-keymap :keymap evil-motion-state-map
    "C-e" #'move-end-of-line
    "C-n" #'next-line
    "C-p" #'previous-line
    "C-v" #'scroll-up-command
    "<down-mouse-1>" nil)

  ;;; Visual state
  (define-keymap :keymap evil-visual-state-map
    "<" #'+evil/shift-left-visual
    ">" #'+evil/shift-right-visual)

  ;;; Evil integrations

  (after! [evil-nerd-commenter]
    (keymap-global-set "<remap> <comment-line>" #'evilnc-comment-or-uncomment-lines))

  ;; (after! [evil-collection]
  ;;   (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  ;;   (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  ;;   (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  ;;     "K" #'magit-discard
  ;;     "L" #'magit-log)
  ;;   (evil-add-hjkl-bindings magit-status-mode-map 'emacs
  ;;     "K" #'magit-discard
  ;;     "l" #'magit-log
  ;;     "h" #'magit-diff-toggle-refine-hunk))

  ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
  ;; `evil-delete' in wgrep buffers.
  (after! 'wgrep
    (keymap-set wgrep-mode-map "<remap> <evil-delete>" #'+evil-delete))

  (after! [evil-matchit]
    ;; TODO: use `keymap-set'
    (define-key evil-normal-state-map "%" #'evilmi-jump-items)
    (define-key evil-visual-state-map "%" #'evilmi-jump-items)
    (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
    (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object))

  (after! [evil-escape]
    ;; TODO: overlaps with global binding above?
    ;; FIXME: this is prob not what we want... don't we want something like `evil-force-normal-state'?
    (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape))

  (after! [evil-visualstar]
    (evil-define-key* 'visual 'global
      "*" #'evil-visualstar/begin-search-forward
      "#" #'evil-visualstar/begin-search-backward))

  (after! [evil-numbers]
    (evil-define-key '(normal visual) 'global (kbd "C-c +") #'evil-numbers/inc-at-pt)
    (evil-define-key '(normal visual) 'global (kbd "C-c -") #'evil-numbers/dec-at-pt)
    (evil-define-key '(normal visual) 'global (kbd "C-c C-+") #'evil-numbers/inc-at-pt-incremental)
    (evil-define-key '(normal visual) 'global (kbd "C-c C--") #'evil-numbers/dec-at-pt-incremental))

  ;;; Text objects

  (after! [evil-indent-plus]
    (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
    (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
    (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
    (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
    (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
    (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

  (after! [evil-args]
    ;; jump out
    ;; FIXME: conflicts with desired K bind
    ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

    ;; inner/outer
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; forward/backward
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)))

(provide 'init-keys-bindings)
;;; init-keys-bindings.el ends here
