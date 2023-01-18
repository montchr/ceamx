;;; init-keys.el --- Keybindings -*- lexical-binding: t -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/dotfield
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

;;; Code:

(elpaca-use-package general
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)

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
    "!"   'shell-command
    ":"   'eval-expression
    "."   'repeat
    "z"   '((lambda (local) (interactive "p")
              (unless repeat-mode (repeat-mode))
              (let ((local current-prefix-arg)
                    (current-prefix-arg nil))
                (call-interactively (if local #'text-scale-adjust #'global-text-scale-adjust))))
            :which-key "zoom"))

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


  ;; Ease the creation of nested menu bindings.
  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    (let* ((n (concat "+general-global-" name))
           (prefix (intern (concat n "-map"))))
      `(progn
         (general-create-definer ,(intern n)
           :wrapping global-definer
           :prefix-map (quote ,prefix)
           :infix ,infix-key
           :wk-full-keys nil
           "" '(:ignore t :which-key ,name))
         (,(intern n) ,@body))))

  (+general-global-menu! "application" "a"
    "p" '(:ignore t "elpaca")
    "pr"  '((lambda () (interactive)
              (let ((current-prefix-arg '(4)))
                (call-interactively #'elpaca-rebuild)))
            :which-key "rebuild")
    "pm" 'elpaca-manager
    "pl" 'elpaca-log
    "pi" '((lambda () (interactive) (info "Elpaca"))
           :which-key "elpaca-info")
    "ps" 'elpaca-status
    "pt" 'elpaca-try
    "pv" 'elpaca-visit)

  (+general-global-menu! "buffer" "b"
    ;; "d"  'kill-current-buffer
    ;; "b" '((lambda () (interactive) (switch-to-buffer nil))
    ;;      :which-key "other-buffer")
    "i" 'ibuffer
    ;; FIXME: broken
    ;; "d" '((lambda () (interactive) (kill-buffer (current-buffer) :wk "delete")))
    "k" 'kill-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n"  'next-buffer
    "N"  '(evil-buffer-new :which-key "new")
    "o" '((lambda () (interactive) (switch-to-buffer nil))
           :which-key "other-buffer")
    "p"  'previous-buffer
    "r"  '(revert-buffer :wk "revert")
    "R"  '(rename-buffer :wk "rename")
    "s"  'save-buffer
    "S"  'save-some-buffers
    "x" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
           :which-key "scratch-buffer")
    "["  'previous-buffer
    "]"  'next-buffer
    "TAB" '((lambda () (interactive) (switch-to-buffer nil))
            :which-key "other-buffer"))

  (+general-global-menu! "bookmark" "B")

  ;; FIXME
  ;;(+general-global-menu! "prev" "["
  ;;  "b" 'prev-buffer)
  ;;(+general-global-menu! "next" "]"
  ;;  "b" 'next-buffer)

  (+general-global-menu! "eval" "e"
    "b" 'eval-buffer
    "d" 'eval-defun
    "e" 'eval-expression
    "p" 'pp-eval-last-sexp
    "s" 'eval-last-sexp)

  (+general-global-menu! "file" "f"
    "f"  'find-file

    "d"   '((lambda (&optional arg)
              (interactive "P")
              (let ((buffer (when arg (current-buffer))))
                (diff-buffer-with-file buffer))) :which-key "diff-with-file")
    ;; TODO
    ;; "D"  '+delete-this-file

    "e"   '(:ignore t :which-key "edit")
    ;;"ed"  '((lambda () (interactive) (find-file-existing literate-file) (widen))
    ;;        :which-key "dotfile")
    "eR"  '((lambda () (interactive) (load-file user-init-file))
            :which-key "reload-init.el")

    "R"   'rename-file-and-buffer
    "s"   'save-buffer
    "S"   '(write-file :wk "Save as ...")
    ;;"u"  #'+sudo-find-file
    ;;    "U"  #'+sudo-this-file
    "v"   'find-variable-at-point
    "V"   'find-variable
    ;;"y"  #'+yank-this-file-name
    )

  (+general-global-menu! "frame" "F"
    "D" 'delete-other-frames
    "F" 'select-frame-by-name
    "O" 'other-frame-prefix
    "c" '(:ingore t :which-key "color")
    "cb" 'set-background-color
    "cc" 'set-cursor-color
    "cf" 'set-foreground-color
    ;; TODO: use fontaine
    ;; "f" 'set-frame-font
    "m" 'make-frame-on-monitor
    "n" 'next-window-any-frame
    "o" 'other-frame
    "p" 'previous-window-any-frame
    "r" 'set-frame-name)

  (+general-global-menu! "git/version-control" "g")

  (+general-global-menu! "help" "h"
    "d"   '(:ignore t :which-key "describe")
    "db"  'describe-bindings
    "df"  'describe-function
    "dF"  'describe-face
    "dk"  'describe-key
    "dt"  '((lambda () (interactive) (describe-text-properties (point)))
            :which-key "describe-text-properties")
    "dv"  'describe-variable

    "h"   (general-simulate-key "C-h" :which-key "help"))

  (+general-global-menu! "link" "l")

  (+general-global-menu! "narrow" "n"
    "d" 'narrow-to-defun
    "p" 'narrow-to-page
    "r" 'narrow-to-region
    "w" 'widen)

  (+general-global-menu! "project" "p"
    "b" '(:ignore t :which-key "buffer"))

  (+general-global-menu! "quit" "q"
    "q" 'save-buffers-kill-emacs
    "r" 'restart-emacs
    "Q" 'kill-emacs)

  (+general-global-menu! "window" "w"
    "?" 'split-window-vertically
    "=" 'balance-windows
    "/" 'split-window-horizontally
    "O" 'delete-other-windows
    "X" '((lambda () (interactive) (call-interactively #'other-window) (kill-buffer-and-window))
          :which-key "kill-other-buffer-and-window")
    "d" 'delete-window
    "h" 'windmove-left
    "j" 'windmove-down
    "k" 'windmove-up
    "l" 'windmove-right
    "o" 'other-window
    "t" '((lambda () (interactive)
            "toggle window dedication"
            (set-window-dedicated-p (selected-window) (not (window-dedicated-p))))
          :which-key "toggle window dedication")
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
           :which-key "divider down")
    "x" 'kill-buffer-and-window)

  ) ;; end `general' configuration

(elpaca-use-package which-key
  :demand t
  :diminish which-key-mode

  :init
  (setq which-key-enable-extended-define-key t)

  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.075)

  :config
  (setq which-key-sort-uppercase-first nil
        which-key-add-column-padding 1)
  (which-key-mode))

;; TODO
;; (when (and env-sys-mac-p env-graphic-p)
;;   (defvar mac-option-modifier)
;;   (defvar mac-command-modifier)
;;   (setq mac-option-modifier nil
;;         mac-command-modifier 'meta))

(provide 'init-keys)
;;; init-keys.el ends here
