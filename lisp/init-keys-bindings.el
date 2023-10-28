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

;; TODO: use `define-prefix-command' instead of aliases

;; I prefer to see everything in one place rather than have bindings
;; scattered through many files. Perhaps I will change my mind someday.

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
;;; Leader maps
;;

;;; Navigation maps

;;;; Previous

(defvar-keymap cmx-go-prev-keymap
  "[" #'previous-buffer
  "b" #'previous-buffer
  "e" #'flycheck-previous-error
  "t" #'tab-previous)
(defalias 'cmx-go-prev-keymap cmx-go-prev-keymap)

;;;; Next

(defvar-keymap cmx-go-next-keymap
  "]" #'next-buffer
  "b" #'next-buffer
  "e" #'flycheck-next-error
  "t" #'tab-next)
(defalias 'cmx-go-next-keymap cmx-go-next-keymap)

;; TODO: make this more convenient
;;;; Goto
(defvar-keymap cmx-goto-keymap
  "d" '("definition" . xref-find-definitions)
  "r" '("references" . xref-find-references))
(defalias 'cmx-goto-keymap cmx-goto-keymap)

;;; "a" => Applications

(defvar-keymap cmx-applications-keymap
  "d" #'dired
  "e" #'eshell
  "m" #'compose-mail
  "n" #'newsticker-show-news)
(defalias 'cmx-applications-keymap cmx-applications-keymap)

;;; "B" => Bookmarks

(defvar-keymap cmx-bookmark-keymap
  "F" #'burly-bookmark-frames
  "W" #'burly-bookmark-windows)
(defalias 'cmx-bookmark-keymap cmx-bookmark-keymap)

;;; "b" => Buffers

(defvar-keymap cmx-buffer-keymap
  "b" `("switch..." . consult-project-buffer)
  "B" '("switch (any)..." . consult-buffer)
  "o" '("other" . mode-line-other-buffer)
  ;; FIXME: only consider file-visiting buffers, or even better, buffers i am editing
  "[" '("prev" . previous-buffer)
  ;; FIXME: only consider file-visiting buffers, or even better, buffers i am editing
  "]" '("next" . next-buffer)
  "r" '("revert" . revert-buffer)
  "R" '("rename..." . rename-buffer)
  "s" '("save" . save-buffer)
  "S" '("save all..." . save-some-buffers)
  "d" '("close" . kill-current-buffer)
  "k" '("close (+win)" . kill-this-buffer)
  ;; FIXME: does not exist
  ;; (keymap-set map "K" ("close others" . kill-other-buffers))
  "M" '("*Messages*" . view-echo-area-messages)
  "x" '("*scratch*" . scratch-buffer))
(defalias 'cmx-buffer-keymap cmx-buffer-keymap)

;;; "c" => Code

(defvar-keymap cmx-code-keymap
  ;; FIXME: wrong type argument commandp error if unavailable
  "a" '("action.." . lsp-execute-code-action)
  "g" '("go to..." . cmx-goto-keymap)
  ;; FIXME: wrong type argument commandp error if unavailable
  "r" '("rename..." . lsp-rename))
(defalias 'cmx-code-keymap cmx-code-keymap)

;;; "e" => Eval

;; FIXME: rename to reflect purpose

(defvar-keymap cmx-elisp-keymap
  "b" #'eval-buffer
  "d" #'eval-defun
  "e" #'eval-last-sexp
  "E" #'eval-expression
  "I" '("reload init-file" . (lambda ()
                               (interactive)
                               (load-file user-init-file)))
  "r" #'eval-region)
(defalias 'cmx-elisp-keymap cmx-elisp-keymap)

;;; "f" => Files

(defvar-keymap cmx-file-keymap
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name
  "C" '("copy..." . cmx/copy-this-file)
  "d" '("diff with..." . cmx/diff-with-file)
  "D" '("delete" . cmx/delete-this-file)
  ;; TODO: show dirvish preview instead of dired preview
  ;; FIXME: flickering on every keystroke...?
  "f" '("find (g)..." . find-file)
  "R" '("rename/move..." . cmx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file))
(defalias 'cmx-file-keymap cmx-file-keymap)

;;; "F" => Frames

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

;;; "g" => Git

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

;;; "h" => Help

(defvar-keymap cmx-help-keymap
  "h" #'help-for-help
  "b" #'embark-bindings
  "f" #'describe-function
  "F" #'describe-face
  "k" #'describe-key
  "l" #'apropos-library
  "o" #'describe-symbol
  "t" '("text properties" . (lambda () (interactive)
                              (describe-text-properties (point))))
  "v" #'describe-variable)
(defalias 'cmx-help-keymap cmx-help-keymap)

;;; "i" => Insertions

(defvar-keymap cmx-insert-keymap
  "t"  #'tempel-insert
  "y"  #'yank-from-kill-ring)
(defalias 'cmx-insert-keymap cmx-insert-keymap)

;;; "n" => Notes

(defvar-keymap cmx-notes-keymap
  "b" #'denote-backlinks
  "d" #'denote-date
  "f" '("find..." . (keymap))
  "f f" #'denote-find-link
  "f b" #'denote-find-backlink
  "i" #'denote-link                     ; "insert" mnemonic
  "I" #'denote-add-links
  "j" #'my-denote-journal               ; our custom command
  "n" #'denote
  "N" #'denote-type
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired buffers.  That is why we bind it here to the `global-map'.
  "r" #'denote-rename-file
  "R" #'denote-rename-file-using-front-matter
  "s" #'denote-subdirectory
  "t" #'denote-template
  "z" #'denote-signature                ; "zettelkasten" mnemonic
  )
(defalias 'cmx-notes-keymap cmx-notes-keymap)


;;; "o" => Org-Mode

;;;; "o c" => Capture
(defvar-keymap cmx-capture-keymap
  "c" '("capture..." . org-capture))
(defalias 'cmx-capture-keymap cmx-capture-keymap)

(defvar-keymap cmx-org-keymap
  "c" '("capture..." . cmx-capture-keymap)
  "t" '("todos" . org-todo-list))
(defalias 'cmx-org-keymap cmx-org-keymap)

;;; "p" => Projects

(defvar-keymap cmx-project-keymap
  "a" '("add..." . projectile-add-known-project)
  "f" '("find file..." . projectile-find-file)
  "i" '("invalidate cache" . projectile-invalidate-cache)
  "p" '("switch..." . projectile-switch-project))
(defalias 'cmx-project-keymap cmx-project-keymap)

;;; "q" => Session

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
  "q" '("save and quit" . save-buffers-kill-emacs)
  "r" '("restart" . restart-emacs)
  "t" '("theme..." . consult-theme))
(defalias 'cmx-session-keymap cmx-session-keymap)

;;; "s" => Search

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

;;; "S" => Sidebars

(defvar-keymap cmx-sidebar-keymap
  "t" #'cmx/treemacs/toggle)
(defalias 'cmx-sidebar-keymap cmx-sidebar-keymap)

;;; "t" => Toggles

(defvar-keymap cmx-toggle-keymap
  "l" #'display-line-numbers-mode
  "L" #'line-number-mode
  "f" #'flycheck-mode
  "t" #'treemacs)
(defalias 'cmx-toggle-keymap cmx-toggle-keymap)

;;; "TAB" => Tabs

(defvar-keymap cmx-tab-keymap
  "d" '("delete" . tab-close)
  "h" '("prev" . tab-previous)
  "l" '("next" . tab-next)
  "n" '("new" . tab-new)
  "t" '("other" . tab-recent)
  "x" '("close" . tab-close))
(defalias 'cmx-tab-keymap cmx-tab-keymap)

;;; Top-level leader map

(defvar-keymap cmx-leader-keymap
  ;; One-shot commands
  "`"    '("other buffer" . mode-line-other-buffer)
  ;; FIXME: make sure this is rebound to `projectile-find-file' once available
  ;; FIXME: should be whatever is bound to "SPC SPC", but using that directly is an error
  "SPC"  #'project-find-file

  "["    '("previous..." . cmx-go-prev-keymap)
  "]"    '("next..." . cmx-go-next-keymap)
  "TAB"  '("tab..." . cmx-tab-keymap)

  "a"		 '("applications..." . cmx-applications-keymap)
  "b"		 '("buffer..." . cmx-buffer-keymap)
  "B"		 '("bookmarks..." . cmx-bookmark-keymap)
  "c"		 '("code..." . cmx-code-keymap)
  ;; TODO: "d"
  "e"		 '("eval..." . cmx-elisp-keymap)
  "f"		 '("file..." . cmx-file-keymap)
  "F"		 '("frame..." . cmx-frame-keymap)
  "g"		 '("git..." . cmx-git-keymap)
  "h"		 '("help..." . cmx-help-keymap)
  "i"		 '("insert..." . cmx-insert-keymap)
  ;; "j"
  ;; "k"
  ;; "l"
  ;; "m"
  "n"		 '("notes..." . cmx-notes-keymap)
  "o"		 '("org..." . cmx-org-keymap)
  "p"		 '("project..." . cmx-project-keymap)
  ;; NOTE: Reserved for `persp-mode-map'.
  "P"  	 '("perspective..." . nil)
  "q"		'("session..." . cmx-session-keymap)
  ;; "r"
  "s"		'("search..." . cmx-search-keymap)
  "S"		'("sidebar..." . cmx-sidebar-keymap)
  "t"		'("toggle..." . cmx-toggle-keymap)
  ;; "u"
  ;; "v"
  "w"		'("window..." . cmx-hydra/window/body))
(defalias 'cmx-leader-keymap cmx-leader-keymap)

;; Make leader keymap bindings accessible under `C-c'.
(set-keymap-parent mode-specific-map cmx-leader-keymap)

(keymap-global-set cmx-leader-alt-key 'cmx-leader-keymap)
(keymap-global-set "<f12>" 'cmx-leader-keymap)

(after! [evil]
  ;; Bind leader key to existing leader map.
  (evil-define-key* '(normal visual motion) 'global (kbd cmx-leader-key) 'cmx-leader-keymap))

;;
;;; Global Bindings
;;

(keymap-global-set "<remap> <keyboard-quit>" #'cmx/escape)

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


  )

(provide 'init-keys-bindings)
;;; init-keys-bindings.el ends here
