;;; init-keys.el --- Keybindings (new) -*- lexical-binding: t -*-

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

;;  Keybindings configuration.

;;; Code:

(keymap-global-set "<f12>" #'cmx-hydra/main/body)

;; macOS: Remap modifier keys.
(when (and +sys-mac-p +graphical-p)
  (setq mac-control-modifier 'control
        mac-option-modifier 'meta
        ns-option-modifier 'meta
        mac-command-modifier 'super
        ns-command-modifier 'super
        ;; Free up the right-side option key for character composition.
        mac-right-option-modifier 'none
        ns-right-option-modifier 'none)
  ;; Common system clipboard hotkeys.
  (global-set-key [(s c)] 'kill-ring-save)
  (global-set-key [(s v)] 'yank)
  (global-set-key [(s x)] 'kill-region)
  (global-set-key [(s q)] 'kill-emacs))

;; HACK: Emacs cannot distinguish between C-i from TAB. This is largely a
;;   byproduct of its history in the terminal, which can't distinguish them
;;   either, however, when GUIs came about Emacs greated separate input events
;;   for more contentious keys like TAB and RET. Therefore [return] != RET,
;;   [tab] != TAB, and [backspace] != DEL.
;;
;;   In the same vein, this keybind adds a [C-i] event, so users can bind to it.
;;   Otherwise, it falls back to regular C-i keybinds.
;; 
;; Source: <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L47-L67>
(define-key key-translation-map [?\C-i]
            (cmd! (if (let ((keys (this-single-command-raw-keys)))
                        (and keys
                             (not (cl-position 'tab    keys))
                             (not (cl-position 'kp-tab keys))
                             (display-graphic-p)
                             ;; Fall back if no <C-i> keybind can be found, otherwise
                             ;; we've broken all pre-existing C-i keybinds.
                             (let ((key
                                    (cmx/lookup-key
                                     (vconcat (cl-subseq keys 0 -1) [C-i]))))
                               (not (or (numberp key) (null key))))))
                      [C-i] [?\C-i])))

;;
;;; Universal, non-nuclear escape
;;  Source: <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L70-L113>

;; `keyboard-quit' is too much of a nuclear option.
;; The following defines a ESC/C-g DWIM alternative
;; It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar cmx-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `cmx/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun cmx/escape (&optional interactive)
  "Run `cmx-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'cmx-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

;; (keymap-global-set "<remap> <keyboard-quit>" #'cmx/escape)

;; (with-eval-after-load 'eldoc
;;   (eldoc-add-command 'cmx/escape))

(defun cmx-meow-define-keys (state &rest keybinds)
  "Define KEYBINDS in STATE.

Adapted from `meow-define-keys' for an interface similar to `defvar-keymap'.

Example usage:
  (cmx-meow-define-keys
    'normal
    \"a\" #'meow-append"
  (declare (indent 1))
  (let ((map (alist-get state meow-keymap-alist)))
    (apply 'define-keymap :keymap map keybinds)))

(defalias 'cmx-meow-normal-define-key
  (apply-partially 'cmx-meow-define-keys 'normal))


;;
;;; which-key
;;

(use-package which-key
  :demand t
  :diminish which-key-mode

  :init
  (setq which-key-enable-extended-define-key t)
  (setq which-key-prefix-prefix "+")
  (setq which-key-separator " ")

  :custom
  (which-key-idle-delay 0.02)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)

  :config
  (setq which-key-sort-uppercase-first nil
        which-key-add-column-padding 1)
  (which-key-mode))

;; Wait until `which-key` is activated so its use-package keyword is installed
(elpaca-wait) 


;;
;;; Bindings
;;
;;; Notes:
;;
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


(cmx-meow-normal-define-key
 "<" #'meow-page-up
 ">" #'meow-page-down)


(defvar-keymap cmx-applications-keymap
  "d" #'dired
  "e" #'eshell
  "m" #'compose-mail
  "n" #'newsticker-show-news)
(defalias 'cmx-applications-keymap cmx-applications-keymap)


(defvar-keymap cmx-bookmark-keymap
  "F" #'burly-bookmark-frames
  "W" #'burly-bookmark-windows)
(defalias 'cmx-bookmark-keymap cmx-bookmark-keymap)


(defvar-keymap cmx-buffer-keymap
  "b" '("switch..." . consult-project-buffer)
  "B" '("switch (any)..." . consult-buffer)
  "o" '("other" . mode-line-other-buffer)
  "[" '("prev" . previous-buffer)
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


(defvar-keymap cmx-code-keymap
  ;; FIXME: wrong type argument commandp error if unavailable
  "a" '("action.." . lsp-execute-code-action)
  ;; FIXME: wrong type argument commandp error if unavailable
  "r" '("rename..." . lsp-rename))
(defalias 'cmx-code-keymap cmx-code-keymap)


(defvar-keymap cmx-elisp-keymap
  "b" #'eval-buffer
  "d" #'eval-defun
  "e" '("eval-last-sexp" . pp-eval-last-sexp)
  "E" #'eval-expression
  "I" '("reload init-file" . (lambda ()
                               (interactive)
                               (load-file user-init-file)))
  "r" #'eval-region)
(defalias 'cmx-elisp-keymap cmx-elisp-keymap)

(defvar-keymap cmx-file-keymap
  ;; TODO
  ;; "u" #'+sudo-find-file
  ;; "U" #'+sudo-this-file
  ;; "y" #'+yank-this-file-name
  "C" '("copy..." . cmx/copy-this-file)
  "d" '("diff with..." . cmx/diff-with-file)
  "D" '("delete" . cmx/delete-this-file)
  ;; TODO: show dirvish preview instead of dired preview
  "f" '("find..." . find-file)
  "R" '("rename/move..." . cmx/move-this-file)
  "s" '("save" . save-buffer)
  "S" '("save as..." . write-file))
(defalias 'cmx-file-keymap cmx-file-keymap)


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


(defvar-keymap cmx-capture-keymap
  "c" '("capture..." . org-capture))
(defalias 'cmx-capture-keymap cmx-capture-keymap)

(defvar-keymap cmx-org-keymap
  "c" '("capture..." . cmx-capture-keymap)
  "t" '("todos" . org-todo-list))
(defalias 'cmx-org-keymap cmx-org-keymap)


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


(defvar-keymap cmx-project-keymap
  "a" '("add..." . projectile-add-known-project)
  "f" '("find file..." . projectile-find-file)
  "i" '("invalidate cache" . projectile-invalidate-cache)
  "p" '("switch..." . projectile-switch-project))
(defalias 'cmx-project-keymap cmx-project-keymap)


(defvar-keymap cmx-session-keymap
  "r" '("restart" . restart-emacs)
  "q" '("save and quit" . save-buffers-kill-emacs))
(defalias 'cmx-session-keymap cmx-session-keymap)


(defvar-keymap cmx-search-keymap
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
  "s" '("line (f)..." . consult-line)
  "v" '("variable" . find-variable-at-point)
  "V" '("variable..." . find-variable)
  "x" '("refs (p)" . projectile-find-references))
(defalias 'cmx-search-keymap cmx-search-keymap)


(defvar-keymap cmx-tab-keymap
  "h" '("prev" . tab-previous)
  "l" '("next" . tab-next)
  "n" '("new" . tab-new)
  "t" '("other" . tab-recent)
  "x" '("close" . tab-close))
(defalias 'cmx-tab-keymap cmx-tab-keymap)


(defvar-keymap cmx-toggle-keymap
  "l" #'display-line-numbers-mode
  "L" #'line-number-mode
  "f" #'flycheck-mode
  "t" #'treemacs)
(defalias 'cmx-toggle-keymap cmx-toggle-keymap)

;; FIXME: this will not work due to meow's d binding
;; (cmx-meow-normal-define-key "g d" #'xref-find-definitions)



(keymap-global-set "C-c `"    '("other buffer" . mode-line-other-buffer))
(keymap-global-set "C-c a"		'("applications..." . cmx-applications-keymap))
(keymap-global-set "C-c b"		'("buffer..." . cmx-buffer-keymap))
(keymap-global-set "C-c B"		'("bookmarks..." . cmx-bookmark-keymap))
;; NOTE: "C-c c" is not easily accessible with meow numpad, hence "C-c C"
;; TODO: Consider using "C-c C-c", which might(?) achieve the desired key sequence ("SPC c")
(keymap-global-set "C-c C"		'("code..." . cmx-code-keymap))
(keymap-global-set "C-c e"		'("eval..." . cmx-elisp-keymap))
(keymap-global-set "C-c f"		'("file..." . cmx-file-keymap))
(keymap-global-set "C-c F"		'("frame..." . cmx-frame-keymap))
(keymap-global-set "C-c G"		'("git..." . cmx-git-keymap))
(keymap-global-set "C-c H"		'("help..." . cmx-help-keymap))
(keymap-global-set "C-c n"		'("notes..." . cmx-notes-keymap))
(keymap-global-set "C-c o"		'("org..." . cmx-org-keymap))
(keymap-global-set "C-c p"		'("project..." . cmx-project-keymap))
(keymap-global-set "C-c P"  	'("packages..." . cmx-packages-keymap))
(keymap-global-set "C-c q"		'("session..." . cmx-session-keymap))
(keymap-global-set "C-c s"		'("search..." . cmx-search-keymap))
(keymap-global-set "C-c TAB"	'("tab..." . cmx-tab-keymap))
(keymap-global-set "C-c t"		'("toggle..." . cmx-toggle-keymap))
(keymap-global-set "C-c w"		'("window..." . cmx-hydra/window/body))


(provide 'init-keys)
;;; init-keys.el ends here
