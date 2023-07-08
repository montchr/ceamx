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
        mac-command-modifier 'super)
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
;;; === GENERAL.EL =============================================================
;;  

(use-package general :demand t)
(elpaca-wait) 

(general-evil-setup)
(general-override-mode)
(general-auto-unbind-keys)

;; FIXME: remove these temporary safeguards against copypasta
(defalias 'gsetq #'general-setq)
(defalias 'gsetq-local #'general-setq-local)
(defalias 'gsetq-default #'general-setq-default)



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
  "`"  '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))) :which-key "prev buffer")
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
  "pr"  '((lambda () (interactive)
            (let ((current-prefix-arg (not current-prefix-arg)))
              (call-interactively #'elpaca-rebuild)))
          :which-key "rebuild")
  "pm" 'elpaca-manager
  "pl" 'elpaca-log
  "pi" '((lambda () (interactive) (info "Elpaca"))
         :which-key "elpaca-info")
  "ps" 'elpaca-status
  "pt" 'elpaca-try
  "pu" 'elpaca-update
  "pU" 'elpaca-update-all
  "pv" 'elpaca-visit)

(+general-global-menu! "buffer" "b"
  "h"  'previous-buffer
  ;; TODO: is there an alternative with better ui? maybe: bufler.el
  "i"  'ibuffer
  "d"  '(kill-current-buffer      :which-key "delete")
  "l"  '(next-buffer              :which-key "next")
  "k"  '(kill-current-buffer      :which-key "kill")
  "M"  '(view-echo-area-messages  :which-key "messages")
  "n"  '(next-buffer              :which-key "next")
  "o"  '(mode-line-other-buffer   :which-key "other...")
  "p"  '(previous-buffer          :which-key "prev")
  "r"  '(revert-buffer            :which-key "revert")
  "R"  '(rename-buffer            :which-key "rename...")
  "s"  '(save-buffer              :which-key "save")
  "S"  'save-some-buffers
  "x"  '(scratch-buffer           :which-key "scratch")

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
  "I" '((lambda () (interactive) (load-file user-init-file))
        :which-key "init.el")
  "p" 'pp-eval-last-sexp
  "r" 'eval-region
  "s" 'eval-last-sexp)

(+general-global-menu! "file" "f"
  "f"  'find-file

  "d"   '((lambda (&optional arg)
            (interactive "P")
            (let ((buffer (when arg (current-buffer))))
              (diff-buffer-with-file buffer))) :which-key "diff-with-file")

  "e"   '(:ignore t :which-key "edit")
  ;; FIXME: eval is what we want
  "eR"  '((lambda () (interactive) (load-file user-init-file))
          :which-key "reload-init.el")

  "s"   'save-buffer
  "S"   '(write-file :which-key "save as...")

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

  "f" '(fontaine-set-preset :which-key "set font...")
  "m" 'make-frame-on-monitor
  "n" 'next-window-any-frame
  "o" 'other-frame
  "p" 'previous-window-any-frame
  "r" 'set-frame-name)

(+general-global-menu! "git/version-control" "g")

(+general-global-menu! "help" "h"
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
  "dt"  '((lambda () (interactive) (describe-text-properties (point)))
          :which-key "text-props")
  "dv"  'describe-variable)

(+general-global-menu! "link" "l")

(+general-global-menu! "narrow" "n"
  "d" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(+general-global-menu! "project" "p"
  "b"  '(:ignore t :which-key "buffer")
  "f"  '(project-find-file :which-key "find-file..."))

(+general-global-menu! "quit" "q"
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs
  "Q" 'kill-emacs)

(+general-global-menu! "search" "s"
  "l"  '((lambda () (interactive "P")
           (call-interactively (if % #'find-library-other-window #'find-library)))
         :which-key "+find-library")
  "v"  'find-variable-at-point
  "V"  'find-variable)

(+general-global-menu! "tabs" "t"
  "n" '(tab-new :which-key "new"))

(+general-global-menu! "toggle" "T"
  "L" '(line-number-mode :which-key "linums")
  "f" '(flycheck-mode :which-key "flycheck"))

(+general-global-menu! "window" "w"
  "?" 'split-window-vertically
  "=" 'balance-windows
  "/" 'split-window-horizontally
  "O" 'delete-other-windows
  "X" '((lambda () (interactive) (call-interactively #'other-window) (kill-buffer-and-window))
        :which-key "kill-other-buffer-and-window")

  "d" 'delete-window
  "D" 'kill-buffer-and-window
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "o" 'other-window
  "t" '((lambda () (interactive)
          "toggle window dedication"
          (set-window-dedicated-p (selected-window) (not (window-dedicated-p))))
        :which-key "toggle window dedication")

  ;; TODO: move to `r' prefix?
  "."  '(:ignore :which-key "resize")
  ".h" '((lambda () (interactive)
           (call-interactively (if (window-prev-sibling) #'enlarge-window-horizontally
                                 #'shrink-window-horizontally)))
         :which-key "divider left")
  ".l" '((lambda () (interactive)
           (call-interactively (if (window-next-sibling) #'enlarge-window-horizontally
                                 #'shrink-window-horizontally)))
         :which-key "divider right")
  ".j" '((lambda () (interactive)
           (call-interactively (if (window-next-sibling) #'enlarge-window #'shrink-window)))
         :which-key "divider up")
  ".k" '((lambda () (interactive)
           (call-interactively (if (window-prev-sibling) #'enlarge-window #'shrink-window)))
         :which-key "divider down"))



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
