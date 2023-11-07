;;; init-keys-bindings.el --- Keybindings            -*- lexical-binding: t; -*-

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

;; TODO: quikgrok descriptions for `cmx-window-keymap' defs
;; TODO: s/cmx-*-keymap/cmx-*-map -- this is Emacs convention (see `define-minor-mode' docs)
;; FIXME: there is no way to update the leader when modifying its composite arm
;;        maps after binding the leader itself, unless you re-bind the leader
;;        key too. or maybe this is caused by using `defalias'?
;; TODO: use `define-prefix-command' instead of aliases
;;       -- though i still haven't figured out how it works

;; I like to see everything in one place rather than have bindings
;; scattered through many files. Perhaps I will change my mind someday.
;; On the other hand, I understand the logic in colocation for the sake of modularity.
;; But this is my brain, not someone else's.

;; While I know I could benefit from using general.el (I have in the past),
;; I am avoiding `general' for a few reasons:
;;
;; - It's bloated and its author has stated they would like to replace it.
;; - While `general' provides a nice interface over many domain complexities,
;;   its *necessary* complexity warrants detailed documentation.
;;   But the docs are pretty *verbose* in addition to being detailed.
;;   It's tough to figure out "idiomatic" usages, especially when
;;   many usages in the wild are using macros/functions
;;   that have been "deprecated" or discouraged.
;; - `evil' already provides its own methods for defining keys, which `general'
;;   still uses under the hood.
;; - GNU Emacs has, especially in version 29, made some improvements
;;   to its core keybinding utilities (e.g. `keymap-set' and `define-keymap').
;;   These almost come close to replicating the syntactic sugar of `general'.
;; - At the end of the day, after I've written a lot of these keybindings
;;   manually with `keymap-set' and `defvar-keymap' etc., I'm coming to grok
;;   many more details and nuances of the keybindings API and packages' usage of them.
;;   So, I am learning by doing things "the hard way".

;;; Tips:

;; - When iterating on keymaps, you can quickly update `which-key' string
;;   replacements by evaling the keymap's `defalias' expression.
;; - Emacs 29 introduces several improved keybinding and keymap functions,
;;   deprecating the long-standing `define-key'-style functions.
;;   Use `keymap-set', `keymap-global-set', `defvar-keymap', and similar.
;;   <https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Keymaps.html>
;; - Despite <https://github.com/justbur/emacs-which-key/issues/338#issuecomment-1101928153>,
;;   the correct string replacements are reflected in which-key output. This may
;;   be thanks to the aliasing of each keymap, as suggested by meow-edit  maintainers.
;;   <https://github.com/meow-edit/meow/issues/71#issuecomment-962090002>
;;   It's also worth noting that this approach comes from Emacs core's
;;   definition of `kmacro-keymap'. However, regardless, the Emacs 29+
;;   keybinding functions also do not cause any issues.

;;; Code:

;;
;;; `cmx-intercept-mode'
;;
;;  Define a minor mode whose associated keymap is registered with
;;  `evil' as an intercept keymap.
;;
;;  <https://github.com/noctuid/evil-guide?tab=readme-ov-file#preventing-certain-keys-from-being-overridden>

(defvar-keymap cmx-intercept-mode-map
  :doc "High-precedence keymap.")

(define-minor-mode cmx-intercept-mode
  "Global minor mode for higher-precedence evil keybindings."
  :global t)

(cmx-intercept-mode)

;; Register with evil.
(after! [evil]
(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap
     cmx-intercept-mode-map state t t)
   state)))

;;
;;; Leader maps
;;

;;; Navigation maps

;; TODO: merge both `cmx-go-prev-keymap' and `cmx-go-next-keymap' into the
;;       appropriate leaderless evil motion state bindings e.g. "[" and "]" in
;;       normal state. i tried doing that quickly but am not sure how to
;;       determine what is bound to "[" or "]" directly. i would have thought
;;       prefix mapped to keymap, but maybe evil is weirder than that...

;;;; Previous

(defvar-keymap cmx-go-prev-keymap
  "TAB" #'tab-previous
  "["   #'previous-buffer
  "b"   #'previous-buffer
  "e"   #'flycheck-previous-error
  "F"   #'previous-window-any-frame
  "t"   #'tab-previous)
(defalias 'cmx-go-prev-keymap cmx-go-prev-keymap)

;;;; Next

(defvar-keymap cmx-go-next-keymap
  "TAB" #'tab-next
  "]"   #'next-buffer
  "b"   #'next-buffer
  "e"   #'flycheck-next-error
  "F"   #'next-window-any-frame
  "t"   #'tab-next)
(defalias 'cmx-go-next-keymap cmx-go-next-keymap)

;; TODO: make this more convenient
;;;; Goto
(defvar-keymap cmx-goto-keymap
  "d" '("definition" . xref-find-definitions)
  "r" '("references" . xref-find-references))
(defalias 'cmx-goto-keymap cmx-goto-keymap)

;;
;;; "B" => Bookmarks
;;

(defvar-keymap cmx-bookmark-keymap
  "F" #'burly-bookmark-frames
  "W" #'burly-bookmark-windows)
(defalias 'cmx-bookmark-keymap cmx-bookmark-keymap)

;;
;;; "b" => Buffers
;;

(defvar-keymap cmx-buffer-keymap
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
(defalias 'cmx-buffer-keymap cmx-buffer-keymap)

;;
;;; "c" => Code
;;

(defvar-keymap cmx-code-keymap
  ;; FIXME: wrong type argument commandp error if unavailable -- language server must support it
  "a" '("action.." . lsp-execute-code-action)
  "r" '("rename..." . lsp-rename)

  "g" '("go to..." . cmx-goto-keymap))
(defalias 'cmx-code-keymap cmx-code-keymap)

;;
;;; "e" => Eval
;;

;; FIXME: rename to reflect purpose

(defvar-keymap cmx-elisp-keymap
  "b" #'eval-buffer
  "d" #'eval-defun
  "e" #'eval-last-sexp
  "E" #'eval-expression
  ;; FIXME: this doesn't actually work as advertised -- in fact, this might not
  ;; be possible, strictly speaking (would need some kind of abstraction for
  ;; user config like that of spacemacs or doom)
  ;; "I" '("reload init-file" . (lambda ()
  ;;                              (interactive)
  ;;                              (load-file user-init-file)))
  "r" #'eval-region)
(defalias 'cmx-elisp-keymap cmx-elisp-keymap)

;;
;;; "f" => Files
;;

(defvar-keymap cmx-file-keymap
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name
  "C" '("copy..." . cmx/copy-this-file)
  "d" '("diff with..." . cmx/diff-with-file)
  ;; FIXME: kill buffer on file deletion
  "D" '("delete" . cmx/delete-this-file)
  ;; TODO: show dirvish preview instead of dired preview
  ;; FIXME: flickering on every keystroke...? prob cos of bad flags (see `init-files')
  "f" '("find (g)..." . find-file)
  "R" '("rename/move..." . cmx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file))
(defalias 'cmx-file-keymap cmx-file-keymap)

;;
;;; "F" => Frames
;;

(defvar-keymap cmx-frame-keymap
  "b" '("save layout..." . burly-bookmark-frames)
	"F" '("switch to..." . select-frame-by-name)
  "n" '("create" . make-frame-on-current-monitor)
  "N" '("create on monitor..." . make-frame-on-monitor)
  "o" '("other" . other-frame)
  "R" '("rename..." . set-frame-name)
  "[" '("prev" . previous-window-any-frame)
  "]" '("next" . next-window-any-frame))
(defalias 'cmx-frame-keymap cmx-frame-keymap)

;;
;;; "g" => Git
;;

(defvar-keymap cmx-git-keymap
  "b" #'magit-branch
  "B" #'magit-blame
  "f" #'magit-find-file
  "g" #'magit-status
  "G" #'magit-dispatch
  "l" #'magit-log-buffer-file
  "s" #'magit-stage-file
  "S" #'magit-unstage-file
  "t" #'git-timemachine)
(defalias 'cmx-git-keymap cmx-git-keymap)

;;
;;; "h" => Help
;;

(defvar-keymap cmx-help-keymap
  "h" #'help-for-help

  "b" #'embark-bindings
  "c" `("cheatsheet..." . ,(cmd! (ido-find-file-in-dir cmx-cheatsheets-dir)))
  "f" #'describe-function
  "F" #'describe-face
  "k" #'describe-key
  "l" #'apropos-library
  "m" #'describe-mode
  "o" #'describe-symbol
  "t" '("text properties" . (lambda () (interactive)
                              (describe-text-properties (point))))
  "v" #'describe-variable)
(defalias 'cmx-help-keymap cmx-help-keymap)

;;
;;; "i" => Insertions
;;

(defvar-keymap cmx-insert-keymap
  "t"  #'tempel-insert
  "y"  #'yank-from-kill-ring)
(defalias 'cmx-insert-keymap cmx-insert-keymap)

;;
;;; "n" => Notes
;;

;; TODO: bind at top-level too
;; TODO: seems redundant but i will likely add a bunch to this
;;;; "n o c" => Org-Capture
(defvar-keymap cmx-capture-keymap
  "c" '("capture..." . org-capture))
(defalias 'cmx-capture-keymap cmx-capture-keymap)

;;;; "n o" => Org-Mode
(defvar-keymap cmx-org-keymap
  "c" '("capture..." . cmx-capture-keymap)
  "t" '("todos" . org-todo-list))
(defalias 'cmx-org-keymap cmx-org-keymap)

(defvar-keymap cmx-notes-keymap
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
  "o" '("[Org-Mode]" . cmx-org-keymap)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired buffers.  That is why we bind it here to the `global-map'.
  "r" #'denote-rename-file
  "R" #'denote-rename-file-using-front-matter
  "s" #'denote-subdirectory
  "t" #'denote-template
  ;; "zettelkasten" mnemonic
  "z" #'denote-signature)
(defalias 'cmx-notes-keymap cmx-notes-keymap)

;;
;;; "o" => Open
;;

(defvar-keymap cmx-open-keymap
  "d" #'dired
  "e" #'eshell
  "l" '("link..." . link-hint-open-link)
  "m" '("mail" . compose-mail)
  "n" '("news" . newsticker-show-news)
  "s" #'suggest)
(defalias 'cmx-open-keymap cmx-open-keymap)

;;
;;; "p" => Projects
;;

(defvar-keymap cmx-project-keymap
  "a" '("add..." . projectile-add-known-project)
  "f" '("find file..." . projectile-find-file)
  "i" '("invalidate cache" . projectile-invalidate-cache)
  "p" '("switch..." . projectile-switch-project))
(defalias 'cmx-project-keymap cmx-project-keymap)

;;
;;; "q" => Session
;;

;;;; "q p" Package Management
(defvar-keymap cmx-packages-keymap
	"i" '("elpaca manual" . (lambda () (interactive)
                            (info "Elpaca")))
  "m" #'elpaca-manager
  "r" #'elpaca-rebuild
  "s" #'elpaca-status
  "t" #'elpaca-try
  "u" #'elpaca-update
  "U" #'elpaca-update-all
  "v" #'elpaca-visit)
(defalias 'cmx-packages-keymap cmx-packages-keymap)

(defvar-keymap cmx-session-keymap
  "f" '("font..." . fontaine-set-preset)
  "p" '("packages" . cmx-packages-keymap)
  "q" '("close frame" . delete-frame)
  "Q" '("save+quit" . save-buffers-kill-emacs)
  "r" '("restart" . restart-emacs)
  "t" '("theme..." . consult-theme))
(defalias 'cmx-session-keymap cmx-session-keymap)

;;
;;; "s" => Search
;;

(defvar-keymap cmx-search-keymap
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
  "p" '("grep (p)..." . consult-ripgrep)
  "R" '("replace (p)..." . projectile-replace)
  "s" '("line (f)..." . consult-line)
  "v" '("variable" . find-variable-at-point)
  "V" '("variable..." . find-variable)
  "x" '("refs (p)" . projectile-find-references))
(defalias 'cmx-search-keymap cmx-search-keymap)

;;
;;; "S" => Sidebars
;;

(defvar-keymap cmx-sidebar-keymap
  "t" #'cmx/treemacs/toggle)
(defalias 'cmx-sidebar-keymap cmx-sidebar-keymap)

;;
;;; "t" => Toggles
;;

(defvar-keymap cmx-toggle-keymap
  "l" #'display-line-numbers-mode
  "L" #'line-number-mode
  "f" #'flycheck-mode
  "t" #'treemacs)
(defalias 'cmx-toggle-keymap cmx-toggle-keymap)

;;
;;; "w" => Window
;;

;; Largely based on Doom bindings, which are based on `evil-window-map'.

(defvar-keymap cmx-window-keymap
  :doc "Custom keymap for window management."

  ;;; default
  "w" #'ace-window

  ;;; tune
  "="  #'balance-windows
  ">"  #'evil-window-increase-width
  "<"  #'evil-window-decrease-width

  ;;; navigate
  "h"      #'evil-window-left
  "j"      #'evil-window-down
  "k"      #'evil-window-up
  "l"      #'evil-window-right
  "C-h"    #'evil-window-left
  "C-j"    #'evil-window-down
  "C-k"    #'evil-window-up
  "C-l"    #'evil-window-right
  "C-w"    #'other-window

  ;;; swap
  "H"      #'cmx/evil/window-move-left
  "J"      #'cmx/evil/window-move-down
  "K"      #'cmx/evil/window-move-up
  "L"      #'cmx/evil/window-move-right
  "r"      #'evil-window-rotate-downwards
  "R"      #'evil-window-rotate-upwards
  "C-S-w"  #'ace-swap-window

  ;;; mutate
  "d"      #'evil-window-delete
  "n"      #'evil-window-new
  "o"      #'delete-other-windows
  "s"      #'evil-window-split
  "T"      #'tear-off-window
  "u"      #'winner-undo
  "v"      #'evil-window-vsplit
  "C-c"    #'ace-delete-window
  "C-r"    #'winner-redo
  "C-u"    #'winner-undo)
(defalias 'cmx-window-keymap cmx-window-keymap)

;;
;;; "y" => Copy / Evil Yank
;;

(defvar-keymap cmx-yank-keymap
  "l" '("link (visible)" . link-hint-copy-link))
(defalias 'cmx-yank-keymap cmx-yank-keymap)

;;
;;; "TAB" => Tabs
;;

(defvar-keymap cmx-tab-keymap
  "TAB"  '("other" . tab-recent)
  "d"    '("delete" . tab-close)
  "h"    '("prev" . tab-previous)
  "l"    '("next" . tab-next)
  "n"    '("new" . tab-new)
  "t"    '("other" . tab-recent)
  "x"    '("close" . tab-close))
(defalias 'cmx-tab-keymap cmx-tab-keymap)


;;
;;; Top-level leader map
;;

(defvar-keymap cmx-leader-keymap
  ;; One-shot commands
  "`"    '("other buffer" . mode-line-other-buffer)
  ;; FIXME: make sure this is rebound to `projectile-find-file' once available
  ;; FIXME: should be whatever is bound to "SPC SPC", but using that directly is an error
  "SPC"  #'project-find-file

  "["    '("[Previous]" . cmx-go-prev-keymap)
  "]"    '("[Next]" . cmx-go-next-keymap)
  "TAB"  '("[Tab]" . cmx-tab-keymap)
  ;; TODO: "a" => Agenda
  "b"		 '("[Buffer]" . cmx-buffer-keymap)
  "B"		 '("[Bookmarks]" . cmx-bookmark-keymap)
  "c"		 '("[Code]" . cmx-code-keymap)
  ;; TODO: "d"
  "e"		 '("[Eval]" . cmx-elisp-keymap)
  "f"		 '("[File]" . cmx-file-keymap)
  "F"		 '("[Frame]" . cmx-frame-keymap)
  "g"		 '("[Git]" . cmx-git-keymap)
  "h"		 '("[Help]" . cmx-help-keymap)
  "i"		 '("[Insert]" . cmx-insert-keymap)
  "j"    '("jump: line" . consult-line)
  ;; "k"
  ;; "l"
  ;; "m"
  "n"		 '("[Notes]" . cmx-notes-keymap)
  "o"		 '("[Open]" . cmx-open-keymap)
  "p"		 '("[Project]" . cmx-project-keymap)
  "q"		 '("[Quit/Session]" . cmx-session-keymap)
  ;; "r"
  "s"		'("[Search]" . cmx-search-keymap)
  ;; TODO: use/lose
  ;; "S"		'("[Sidebar]" . cmx-sidebar-keymap)
  "t"		'("[Toggle]" . cmx-toggle-keymap)
  ;; "u"
  ;; "v"
  "w"		'("[Window]" . cmx-window-keymap)
  "x"   '("[Capture]" . cmx-capture-keymap)
  "y"   '("[Copy]" . cmx-yank-keymap)
  ;; "z"
  )
(defalias 'cmx-leader-keymap cmx-leader-keymap)

;; Make leader keymap bindings accessible under `C-c',
;; while keeping other commands bound to `mode-specific-map'.
(set-keymap-parent mode-specific-map cmx-leader-keymap)

(keymap-global-set cmx-leader-alt-key 'cmx-leader-keymap)
(keymap-global-set "<f12>" 'cmx-leader-keymap)

(after! [evil]
  ;; Bind leader key to existing leader map.
  ;; FIXME: overridden in magit-status
  (evil-define-key* '(normal visual motion) 'global (kbd cmx-leader-key) 'cmx-leader-keymap)

  ;; EXPERIMENTAL: Bind leader to `,' (comma).
  (evil-define-key* '(normal visual motion) 'global "," 'cmx-leader-keymap)
  )

;;
;;; Global Bindings
;;

(keymap-global-set "<remap> <keyboard-quit>" #'cmx/escape)

;; macOS muscle-memory habits
(when +sys-mac-p
  (keymap-global-set "s-{" #'tab-previous)
  (keymap-global-set "s-}" #'tab-next))

(dolist (key '("M-["
               "M-{"
               "M-\""
               "M-'"
               "M-`"))
  (keymap-global-set key #'insert-pair))

(keymap-global-set "C-x SPC" #'hydra-rectangle/body)

(after! [vundo]
  (keymap-global-set "C-x u" #'vundo))

;;
;;; Evil Bindings
;;

(after! [evil]
  ;; Don't let evil trample over the immensely-useful question mark.
  (evil-define-key '(normal visual motion) 'global "?" nil)

  ;;; Normal state
  (define-keymap :keymap evil-normal-state-map
    "g d"  #'xref-find-definitions
    "K"    #'helpful-at-point)
  ;; (evil-define-key* '(normal) 'global "K" #'helpful-at-point)

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

  (after! [wgrep]
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
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

  ;; TODO: DRY
  (after! [evil-args]
    ;; inner/outer
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; forward/backward
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; jump out
    ;; FIXME: conflicts with desired K bind
    ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)
    )

  ) ; end `(after! [evil])'

(provide 'init-keys-bindings)
;;; init-keys-bindings.el ends here
