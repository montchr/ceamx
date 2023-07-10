;;; init-keybinds.el --- Keybindings -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Keybindings configuration
;;
;; <https://github.com/noctuid/general.el#usage-recommendations-and-documentation-clarifications>

;;; Code:

(require 'lib-keybinds)


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

(global-set-key [remap keyboard-quit] #'cmx/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'cmx/escape))


;;
;;; === GENERAL.EL =============================================================
;;  


;;; TODO: move vars to top of file

(defvar cmx-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar cmx-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar cmx-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar cmx-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")

(defvar cmx-hydra-key "<f12>"
  "Key to bind `cmx/body' to.")


;;; --- load ---

(use-package general :demand t)
(elpaca-wait) 

;; Create alias macros corresponding to vimisms.
;; <https://github.com/noctuid/general.el/blob/feature/general-config-and-speed-documentation/README.org#vim-like-definers>
;; TODO: consider removal, i don't need this, but ensure it's unused
(general-evil-setup)


;;; --- aliases / macros ---

;; FIXME: remove these temporary safeguards against copypasta
(defalias 'gsetq #'general-setq)
(defalias 'gsetq-local #'general-setq-local)
(defalias 'gsetq-default #'general-setq-default)
;; TODO: maybe remove (added as copypasta safeguard)
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)

;; Prevent "X starts with non-prefix key Y" errors except at startup.
;; via doom <https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/lisp/doom-keybinds.el#L123-L124>
;; (add-hook 'doom-after-modules-init-hook #'general-auto-unbind-keys)

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `cmx-localleader-key' and `cmx-localleader-alt-key' to change the
localleader prefix."
  `(general-define-key
    :states '(normal visual motion emacs insert)
    :major-modes t
    :prefix cmx-localleader-key
    :non-normal-prefix cmx-localleader-alt-key
    ,@args))

;; PERF: `evil-define-key*' instead of `general-define-key' (via doom)
(evil-define-key* '(normal visual motion) map (kbd cmx-leader-key) 'cmx/body)
(evil-define-key* '(emacs insert) map (kbd cmx-leader-alt-key) 'cmx/body)

(general-override-mode)
(general-auto-unbind-keys)



;;; -----------------------------------------------------

(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator emacs)
 :prefix-map '+prefix-map
 :prefix "SPC"
 :global-prefix "S-SPC")

(general-create-definer global-definer
  :wk-full-keys nil
  :keymaps '+prefix-map)

;; leaderless bindings
(global-definer
  "`"  '("other buffer" . (lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))))
  "!"  'shell-command
  ":"  'eval-expression
  "."  'repeat
  "SPC"  'project-find-file)

;; major modes
(general-create-definer global-leader
  :keymaps 'override
  :states '(insert normal hybrid motion visual operator)
  :prefix "SPC m"
  :non-normal-prefix "S-SPC m"
  "" '( :ignore t
        :which-key
        (lambda (arg)
          (cons (cadr (split-string (car arg) " "))
                (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

(defalias 'kbd! #'general-simulate-key)

;; Ease the creation of nested menu bindings.
;; TODO: define sparse keymaps so that menus whose mappings are all deferred
;;       do not result in an empty menu (this is a hypothesis though)
(defmacro +general-global-menu! (name prefix-key &rest body)
  "Create a definer named +general-global-NAME wrapping global-definer.
  Create prefix map: +general-global-NAME-map. Prefix bindings in BODY with PREFIX-KEY."
  (declare (indent 2))
  (let* ((n (concat "+general-global-" name))
         (prefix-map (intern (concat n "-map"))))
    `(progn
       (general-create-definer ,(intern n)
         :wrapping global-definer
         :prefix-map (quote ,prefix-map)
         :prefix ,prefix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern n) ,@body))))

(+general-global-menu! "application" "a"
  "p" '(:ignore t "elpaca")
  "pr"  '("rebuild" . (lambda () (interactive)
                        (let ((current-prefix-arg (not current-prefix-arg)))
                          (call-interactively #'elpaca-rebuild))))
  "pm" 'elpaca-manager
  "pl" 'elpaca-log
  "pi" '("elpaca-info" . (lambda () (interactive) (info "Elpaca")))
  "ps" 'elpaca-status
  "pt" 'elpaca-try
  "pu" 'elpaca-update
  "pU" 'elpaca-update-all
  "pv" 'elpaca-visit)

(+general-global-menu! "buffer" "b"
  "h"  '("prev" . previous-buffer)
  ;; TODO: is there an alternative with better ui? maybe: bufler.el
  "i"  'ibuffer
  "d"  '("delete" . kill-current-buffer)
  "l"  '("next" . next-buffer)
  "k"  '("kill" . kill-current-buffer)
  "M"  '("messages" . view-echo-area-messages)
  "n"  '("next" . next-buffer)
  "o"  '("other..." . mode-line-other-buffer)
  "p"  '("prev" . previous-buffer)
  "r"  '("revert" . revert-buffer)
  "R"  '("rename..." . rename-buffer)
  "s"  '("save" . save-buffer)
  "S"  #'save-some-buffers
  "x"  '("scratch" . scratch-buffer)

  "["  'previous-buffer
  "]"  'next-buffer
  "TAB"  'mode-line-other-buffer)

(+general-global-menu! "bookmark" "B")

(+general-global-menu! "code" "c")

;; TODO: move to `init-lang-elisp'?
(+general-global-menu! "eval" "e"
  "b" 'eval-buffer
  "d" 'eval-defun
  "e" 'elisp-eval-region-or-buffer
  "E" 'eval-expression
  "I" '("init.el" . (lambda () (interactive) (load-file user-init-file)))
  "p" 'pp-eval-last-sexp
  "r" 'eval-region
  "s" 'eval-last-sexp)

(+general-global-menu! "file" "f"
  "f"  'find-file

  "d"   '("diff with file" . (lambda (&optional arg)
                               (interactive "P")
                               (let ((buffer (when arg (current-buffer))))
                                 (diff-buffer-with-file buffer))))

  ;; "e"   '(:ignore t :which-key "edit")
  ;; FIXME: eval is what we want
  ;; "eR"  '("reload config" (lambda () (interactive) (load-file user-init-file)))

  "s"   'save-buffer
  "S"   '("save as..." . write-file)

  ;; TODO
  ;;"u"  #'+sudo-find-file
  ;;    "U"  #'+sudo-this-file
  ;;"y"  #'+yank-this-file-name

  )

(+general-global-menu! "frame" "F"
  "F" 'select-frame-by-name

  "D" 'delete-other-frames
  "O" 'other-frame-prefix

  ;; FIXME: replace with theme / `modus-themes' bindings
  "c" '(:ignore t :which-key "color")
  "cb" 'set-background-color
  "cc" 'set-cursor-color
  "cf" 'set-foreground-color

  "f" '("set font..." . fontaine-set-preset)
  "m" 'make-frame-on-monitor
  "n" 'next-window-any-frame
  "o" 'other-frame
  "p" 'previous-window-any-frame
  "r" 'set-frame-name)

(+general-global-menu! "git/version-control" "g")

(+general-global-menu! "help" "h"
  ;; FIXME: this seems suspect...
  "h"  (kbd! "C-h" :which-key "help")

  "b"  'describe-bindings
  "f"  'describe-function
  "F"  'describe-face
  "k"  'describe-key
  "l"  'apropos-library
  "o"  'describe-symbol
  "v"  'describe-variable

  ;; FIXME: somehow these inherit the `which-key' labels from the top-level leader menu
  "d"   '(:ignore t :which-key "describe")
  "db"  'describe-bindings
  "df"  'describe-function
  "dF"  'describe-face
  "dk"  'describe-key
  "dt"  '("text props" . (lambda () (interactive)
                           (describe-text-properties (point))))
  "dv"  'describe-variable)

(+general-global-menu! "link" "l")

(+general-global-menu! "narrow" "n"
  "d" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(+general-global-menu! "project" "p"
  "b"  '(:ignore t :which-key "buffer")
  "f"  '("find-file..." . project-find-file))

(+general-global-menu! "quit" "q"
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs
  "Q" 'kill-emacs)

(+general-global-menu! "search" "s"
  "l"  '("+find-library" .
         (lambda () (interactive "P")
           (call-interactively
            (if % #'find-library-other-window #'find-library))))
  "v"  'find-variable-at-point
  "V"  'find-variable)

(+general-global-menu! "tabs" "t"
  "n" '("new" . tab-new))

(+general-global-menu! "toggle" "T"
  "L" '("linums" . line-number-mode)
  "f" '("flycheck" . flycheck-mode))

(+general-global-menu! "window" "w"
  "?" 'split-window-vertically
  "=" 'balance-windows
  "/" 'split-window-horizontally
  "O" 'delete-other-windows
  "X" '("kill-other-buffer-and-window" .
        (lambda () (interactive)
          (call-interactively #'other-window)
          (kill-buffer-and-window)))
  "d" 'delete-window
  "D" 'kill-buffer-and-window
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "o" 'other-window
  "t" '("toggle window dedication" .
        (lambda () (interactive)
          "toggle window dedication"
          (set-window-dedicated-p
           (selected-window)
           (not (window-dedicated-p)))))

  ;; TODO: move to `r' prefix?
  "."  '(:ignore :which-key "resize")
  ".h" '("divider left" .
         (lambda () (interactive)
           (call-interactively
            (if (window-prev-sibling)
                #'enlarge-window-horizontally
              #'shrink-window-horizontally))))

  ".l" '("divider right" .
         (lambda () (interactive)
           (call-interactively
            (if (window-next-sibling)
                #'enlarge-window-horizontally
              #'shrink-window-horizontally))))

  ".j" '("divider up" .
         (lambda () (interactive)
           (call-interactively
            (if (window-next-sibling)
                #'enlarge-window
              #'shrink-window))))

  ".k" '("divider down" .
         (lambda () (interactive)
           (call-interactively
            (if (window-prev-sibling)
                #'enlarge-window
              #'shrink-window)))))



;;
;;; === WHICH-KEY ==============================================================
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
;;; === BINDINGS ===============================================================
;;  


(provide 'init-keybinds)
;;; init-keybinds.el ends here
