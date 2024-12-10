;;; init-window.el --- Window management  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@protonmail.com>

;; Author: Chris Montgomery <chmont@protonmail.com>
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
;;; Code:

(require 'transient)

(require 'config-buffer)
(require 'config-window)

(require 'ceamx-lib)
(require 'lib-buffer)
(require 'lib-window)

;; General buffer display settings :buffer:frame:display_buffer:


;; [[file:../config.org::*General buffer display settings][General buffer display settings:1]]
(setopt switch-to-buffer-in-dedicated-window 'pop)

;; Ensure interactive buffer switching behaves according to expectations.
(setopt switch-to-buffer-obey-display-actions t)

;; Hide buffer until there's output.
;; Prevents an extra window appearing during init.
(setopt async-shell-command-display-buffer nil)

;; TODO: causes which-key squishing against tiny window maybe?
(setopt fit-window-to-buffer-horizontally t)

;; TODO: this might be a solution to issues with childframes for embark etc.
(setopt fit-frame-to-buffer t)

;; (setopt even-window-sizes nil)
(setopt even-window-sizes 'height-only)
(setopt window-combination-resize t)
(setopt window-sides-vertical nil)
(setopt window-resize-pixelwise t)

(setopt display-buffer-base-action
        '((display-buffer-reuse-window
           display-buffer-in-previous-window)))
;; General buffer display settings:1 ends here

;; Declare rules for displaying buffers with ~display-buffer-alist~ :display_buffer:

;; - Source :: <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

;; <karthink> has a helpful summary of ~display-buffer~ action functions and
;; alist entries in their Emacs configuration, which I am also including here
;; for my own reference. Note that this list is not necessarily complete.

;; ~display-buffer-action-functions~ are:

;; - ~display-buffer-same-window~ :: Use the selected window
;; - ~display-buffer-reuse-window~ :: Use a window already showing the buffer
;; - ~display-buffer-reuse-mode-window~ :: Use a window with the same major-mode
;; - ~display-buffer-in-previous-window~ :: Use a window that did show the buffer before
;; - ~display-buffer-use-some-window~ :: Use some existing window
;; - ~display-buffer-pop-up-window~ :: Pop up a new window
;; - ~display-buffer-below-selected~ :: Use or pop up a window below the selected one
;; - ~display-buffer-at-bottom~ :: Use or pop up a window at the bottom of the selected frame
;; - ~display-buffer-pop-up-frame~ :: Show the buffer on a new frame
;; - ~display-buffer-in-child-frame~ :: Show the buffer in a child frame
;; - ~display-buffer-no-window~ :: Do not display the buffer and have ~display-buffer~ return nil immediately

;; Action alist entries are:

;; - ~inhibit-same-window~ :: A non-nil value prevents the sam
;;     window from being used for display
;; - ~inhibit-switch-frame~ :: A non-nil value prevents any fram
;;     used for showing the buffer from being raised or selected
;; - ~reusable-frames~ :: The value specifies the set of frames t
;;     search for a window that already displays the buffer.
;;     Possible values are nil (the selected frame), t (any live
;;     frame), visible (any visible frame), 0 (any visible or
;;     iconified frame) or an existing live frame.
;; - ~pop-up-frame-parameters~ :: The value specifies an alist o
;;     frame parameters to give a new frame, if one is created.
;; - ~window-height~ :: The value specifies the desired height of th
;;     window chosen and is either an integer (the total height of
;;     the window), a floating point number (the fraction of its
;;     total height with respect to the total height of the frame's
;;     root window) or a function to be called with one argument -
;;     the chosen window.  The function is supposed to adjust the
;;     height of the window; its return value is ignored.  Suitable
;;     functions are ~shrink-window-if-larger-than-buffer~ and
;;     ~fit-window-to-buffer~.
;; - ~window-width~ :: The value specifies the desired width of th
;;     window chosen and is either an integer (the total width of
;;     the window), a floating point number (the fraction of its
;;     total width with respect to the width of the frame's root
;;     window) or a function to be called with one argument - the
;;     chosen window.  The function is supposed to adjust the width
;;     of the window; its return value is ignored.
;; - ~preserve-size~ :: The value should be either (t . nil) t
;;     preserve the width of the chosen window, (nil . t) to
;;     preserve its height or (t . t) to preserve its height and
;;     width in future changes of the window configuration.
;; - ~window-parameters~ :: The value specifies an alist of windo
;;     parameters to give the chosen window.
;; - ~allow-no-window~ :: A non-nil value means that `display-buffer
;;     may not display the buffer and return nil immediately.


;;     <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>



;; [[file:../config.org::*Declare rules for displaying buffers with ~display-buffer-alist~][Declare rules for displaying buffers with ~display-buffer-alist~:1]]
(require 'lib-buffer)

;; TODO: move these to config-...
(defvar ceamx-checkers-buffer-names-regexp
  (rx "*" (or "Flycheck" "Package-Lint")))

(setopt display-buffer-alist
        `(
          ;; (,(rx "*" (or "Agenda Commands" "Org Select") "*")
          ;;   (display-buffer-below-selected
          ;;     display-buffer-in-side-window)
          ;;   (body-function . select-window)
          ;;   (window-parameters . ((mode-line-format . nil))))

          (,ceamx-checkers-buffer-names-regexp
           (display-buffer-in-direction
            display-buffer-in-side-window)
           (window-parameters . ((no-other-window . t))))

          ;; TODO: is there not a simpler way than using `ceamx-buffer-mode'?
          ;; e.g. `derived-mode-p' or similar
          ((lambda (buf act) (member (ceamx-buffer-mode buf) ceamx-message-modes-list))
           (display-buffer-at-bottom
            display-buffer-in-side-window))

          (,(rx "*" (group (or "Compile-Log" "Messages" "Warnings")) "*")
           (display-buffer-at-bottom
            display-buffer-in-side-window
            display-buffer-in-direction))

          (,(rx "*Backtrace*")
           (display-buffer-in-side-window)
           (window-height . 0.2)
           (side . bottom))))
;; Declare rules for displaying buffers with ~display-buffer-alist~:1 ends here

;; ~popper~: Summon and dismiss "popup" windows :popups:package:

;; - Website :: <https://github.com/karthink/popper>


;; [[file:../config.org::*~popper~: Summon and dismiss "popup" windows][~popper~: Summon and dismiss "popup" windows:1]]
(package! popper
  (global-keys!
    "C-`" #'popper-toggle
    "C-~" #'popper-cycle
    "C-M-`" #'popper-toggle-type)

  (setopt popper-reference-buffers
          (append
           ceamx-help-modes-list
           ceamx-help-buffer-names-list
           ceamx-manual-modes-list
           ceamx-repl-modes-list
           ceamx-repl-buffer-names-list
           ceamx-occur-grep-modes-list
           '(+popper-current-buffer-popup-p)
           '(Custom-mode
             compilation-mode
             messages-buffer-mode)
           (list
            ceamx-checkers-buffer-names-regexp)

           ;; The "Home" tabspace, if enabled, will display the Messages buffer.
           (unless (fboundp 'ceamx-workspace-open-tabspace-after-init-h)
             '("\\*Messages\\*"))

           `(,(rx "Output*" eol)
             ,(rx "*" (or
                       "Async-native-compile-log"
                       "Backtrace"
                       "Compile-Log"
                       "Completions"
                       "compilation"
                       "elpaca-diff"
                       "Shell Command Output"
                       "vc"
                       "Warnings")
               "*")
             "^\\*Embark Export"
             "^Calc:"
             "\\*Async Shell Command\\*"
             ;; ("\\*Async Shell Command\\*" . hide)
             ("\\*Detached Shell Command\\*" . hide))))

  ;; Load as early as possible to catch popups as early as possible.
  (popper-mode)
  (popper-echo-mode))
;; ~popper~: Summon and dismiss "popup" windows:1 ends here

;; Configure overrides in ~popper-repeat-map~


;; [[file:../config.org::*Configure overrides in ~popper-repeat-map~][Configure overrides in ~popper-repeat-map~:1]]
(after! popper
  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards))
;; Configure overrides in ~popper-repeat-map~:1 ends here

;; Configure ~projectile~ integration


;; [[file:../config.org::*Configure ~projectile~ integration][Configure ~projectile~ integration:1]]
(after! (popper projectile)
  (setopt popper-group-function #'popper-group-by-projectile))
;; Configure ~projectile~ integration:1 ends here

;; Restore previous window configurations with ~winner-mode~ [builtin] :history:


;; [[file:../config.org::*Restore previous window configurations with ~winner-mode~ \[builtin\]][Restore previous window configurations with ~winner-mode~ [builtin]:1]]
(add-hook 'ceamx-after-init-hook #'winner-mode)
;; Restore previous window configurations with ~winner-mode~ [builtin]:1 ends here

;; =dedicated=: Toggle a window's "dedicated" flag :package:

;; <https://github.com/emacsorphanage/dedicated/tree/f47b504c0c56fa5ab9d1028417ca1f65a713a2f0>


;; [[file:../config.org::*=dedicated=: Toggle a window's "dedicated" flag][=dedicated=: Toggle a window's "dedicated" flag:1]]
(package! dedicated
  (keymap-global-set "C-c W" #'dedicated-mode))
;; =dedicated=: Toggle a window's "dedicated" flag:1 ends here

;; =olivetti=: "Distraction-free" editing :package:

;; <https://github.com/rnkn/olivetti>


;; [[file:../config.org::*=olivetti=: "Distraction-free" editing][=olivetti=: "Distraction-free" editing:1]]
(package! olivetti)

;;  (setopt olivetti-style 'fancy) ; might not play well with `org-modern'
;; =olivetti=: "Distraction-free" editing:1 ends here

;; =golden-ratio=: Automatically resize windows according to Ancient Wisdom :package:


;; [[file:../config.org::*=golden-ratio=: Automatically resize windows according to Ancient Wisdom][=golden-ratio=: Automatically resize windows according to Ancient Wisdom:1]]
(package! golden-ratio
  (setopt golden-ratio-auto-scale t)
  (setopt golden-ratio-max-width 100))
;; =golden-ratio=: Automatically resize windows according to Ancient Wisdom:1 ends here

;; =ace-window=: Interactively manage windows :package:

;; <https://github.com/abo-abo/ace-window>


;; [[file:../config.org::*=ace-window=: Interactively manage windows][=ace-window=: Interactively manage windows:1]]
(package! ace-window
  ;; Same frame only. While it'd be nice to use the default (global), I really
  ;; dislike that it orders window numbers leads to jarring gaps in window
  ;; numbers in the same frame. For example, frame A might have windows numbered
  ;; 1 and 3 and frame B will have window 2.
  (setopt aw-scope 'frame))
;; =ace-window=: Interactively manage windows:1 ends here

;; =transpose-frame=: Transpose and rotate a frame's windows :package:


;; [[file:../config.org::*=transpose-frame=: Transpose and rotate a frame's windows][=transpose-frame=: Transpose and rotate a frame's windows:1]]
(keymap-global-set "C-c w" (cons "Window" (define-prefix-command 'ceamx-custom-x-prefix)))

(package! transpose-frame
  (keymap-global-set "C-c w SPC" #'transpose-frame))
;; =transpose-frame=: Transpose and rotate a frame's windows:1 ends here

;; ~ceamx/window-dispatch~: a window-management menu :transient:menu:keybinds:


;; [[file:../config.org::*~ceamx/window-dispatch~: a window-management menu][~ceamx/window-dispatch~: a window-management menu:1]]
(transient-define-prefix ceamx/window-dispatch ()
  "Window management transient."
  :transient-suffix 'transient--do-stay
  [["Move"
    ("h" "left" windmove-left)
    ("j" "down" windmove-down)
    ("k" "up" windmove-up )
    ("l" "right" windmove-right)
    ("w" "sel" ace-window)]

   ["Resize"
    ("=" "bal" balance-windows)
    ("+" "bal: area" balance-windows-area)
    ("-" "fit: buffer" fit-window-to-buffer)]

   ["Buffer"
    ("b" "buf" consult-buffer)
    ;; ("f" "ff: p" project-find-file)
    ("f" "file" find-file )
    ("F" "file" find-file-other-window)
    ("g" "grep" consult-ripgrep)]

   ["Swarp"
    ("H" "left" ceamx/window-move-left)
    ("J" "down" ceamx/window-move-down)
    ("K" "up" ceamx/window-move-up)
    ("L" "right" ceamx/window-move-right)
    ""
    ("s" "swap" ace-swap-window)
    ("2" "spl: dn" split-window-below)
    ("3" "spl: rt" split-window-right)
    ("SPC" "swap-or-rotate" ceamx/swap-or-rotate-windows)]

   ["Scroll"
    ;; TODO: allow selecting a window (with infix?) to act upon
    ;; NOTE: These are the correct scroll direction commands, which might
    ;; appear to be reversed when comparing with labels.
    ("." "left" scroll-right)
    ("," "right" scroll-left)
    ("SPC" "down" scroll-up)
    ("DEL" "up" scroll-down)]

   ["Lifecycle"
    ("d" "del (this)" delete-window)
    ("D" "del (select)" ace-delete-window)
    ;; ("D" "del: o" delete-other-windows :transient nil)
    ("u" "undo" winner-undo)
    ("U" "redo" winner-redo)
    ""
    ("0" "del" delete-window)
    ("1" "del other" delete-other-windows)
    ""
    ("S" "[ ] sides" window-toggle-side-windows)
    ("`" "[ ] popups" popper-toggle)
    ""
    ("q" "quit" transient-quit-all)]])
;; ~ceamx/window-dispatch~: a window-management menu:1 ends here

;; Bind additional ~window-prefix-map~ keys (~window-prefix-map~) :keybinds:


;; [[file:../config.org::*Bind additional ~window-prefix-map~ keys (~window-prefix-map~)][Bind additional ~window-prefix-map~ keys (~window-prefix-map~):1]]
(define-keymap :keymap window-prefix-map
  "w" #'ace-window

  "d" #'ace-delete-window
  "p" #'popper-toggle
  "P" #'popper-toggle-type
  "u" #'winner-undo

  "h" #'windmove-left
  "H" #'ceamx/window-move-left
  "j" #'windmove-down
  "J" #'ceamx/window-move-down
  "k" #'windmove-up
  "K" #'ceamx/window-move-up
  "l" #'windmove-right
  "L" #'ceamx/window-move-right

  "=" #'balance-windows
  "SPC" #'ceamx/swap-or-rotate-windows)
;; Bind additional ~window-prefix-map~ keys (~window-prefix-map~):1 ends here

;; Bind additional global keys :keybinds:


;; [[file:../config.org::*Bind additional global keys][Bind additional global keys:1]]
(define-keymap :keymap (current-global-map)
  "C-x o" #'ceamx/other-window
  "C-x O" #'ace-window

  "C-x =" #'balance-windows
  "C-x +" #'balance-windows-area

  "C-x C-n" #'next-buffer
  "C-x C-p" #'previous-buffer

  "C-x <up>" #'enlarge-window           ; also: C-x ^
  "C-x <down>" #'shrink-window
  "C-x <left>" #'shrink-window-horizontally
  "C-x <right>" #'enlarge-window-horizontally)
;; Bind additional global keys:1 ends here

;; Bind repeatable keys for resizing windows (~resize-window-repeat-map~) :keybinds:


;; [[file:../config.org::*Bind repeatable keys for resizing windows (~resize-window-repeat-map~)][Bind repeatable keys for resizing windows (~resize-window-repeat-map~):1]]
(define-keymap :keymap resize-window-repeat-map
  "<up>" #'enlarge-window
  "<down>" #'shrink-window
  "<left>" #'shrink-window-horizontally
  "<right>" #'enlarge-window-horizontally)
;; Bind repeatable keys for resizing windows (~resize-window-repeat-map~):1 ends here

;; Bind repeatable keys for window actions (~ceamx-window-repeat-map~) :keybinds:

;; This is very similar to ~window-prefix-map~.  Unfortunately, it does not seem
;; possible to create a functional ~repeat-map~ inheriting from a parent keymap.  The
;; commands bound in the parent map are unaffected by the ~defvar-keymap~ =:repeat=
;; keyword of a child map.


;; [[file:../config.org::*Bind repeatable keys for window actions (~ceamx-window-repeat-map~)][Bind repeatable keys for window actions (~ceamx-window-repeat-map~):1]]
(defvar-keymap ceamx-window-repeat-map
  :repeat t

  "0" #'delete-window
  "2" #'split-window-below
  "3" #'split-window-right

  "b" #'consult-buffer
  "f" #'find-file

  "o" #'ceamx/other-window
  "P" #'popper-toggle-type
  "u" #'winner-undo

  "h" #'windmove-left
  "H" #'ceamx/window-move-left
  "j" #'windmove-down
  "J" #'ceamx/window-move-down
  "k" #'windmove-up
  "K" #'ceamx/window-move-up
  "l" #'windmove-right
  "L" #'ceamx/window-move-right

  "SPC" #'ceamx/swap-or-rotate-windows

  "RET" #'repeat-exit
  "ESC" #'repeat-exit)
;; Bind repeatable keys for window actions (~ceamx-window-repeat-map~):1 ends here

(provide 'init-window)
;;; init-window.el ends here
