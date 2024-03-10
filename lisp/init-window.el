;;; init-window.el --- Window management -*- lexical-binding: t -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chris@cdom.io>

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

;; <karthink> has a helpful summary of `display-buffer' action functions and
;; alist entries in their Emacs configuration, which I am also including here
;; for my own reference. Note that this list is not necessarily complete.

;;;; display-buffer-action-functions are:
;;  `display-buffer-same-window' -- Use the selected window.
;;  `display-buffer-reuse-window' -- Use a window already showing the buffer.
;;  `display-buffer-reuse-mode-window' -- Use a window with the same major-mode.
;;  `display-buffer-in-previous-window' -- Use a window that did show the buffer before.
;;  `display-buffer-use-some-window' -- Use some existing window.
;;  `display-buffer-pop-up-window' -- Pop up a new window.
;;  `display-buffer-below-selected' -- Use or pop up a window below the selected one.
;;  `display-buffer-at-bottom' -- Use or pop up a window at the bottom of the selected frame.
;;  `display-buffer-pop-up-frame' -- Show the buffer on a new frame.
;;  `display-buffer-in-child-frame' -- Show the buffer in a child frame.
;;  `display-buffer-no-window' -- Do not display the buffer and have `display-buffer' return nil immediately.

;;;; Action alist entries are:
;;  `inhibit-same-window' -- A non-nil value prevents the same
;;     window from being used for display.
;;  `inhibit-switch-frame' -- A non-nil value prevents any frame
;;     used for showing the buffer from being raised or selected.
;;  `reusable-frames' -- The value specifies the set of frames to
;;     search for a window that already displays the buffer.
;;     Possible values are nil (the selected frame), t (any live
;;     frame), visible (any visible frame), 0 (any visible or
;;     iconified frame) or an existing live frame.
;;  `pop-up-frame-parameters' -- The value specifies an alist of
;;     frame parameters to give a new frame, if one is created.
;;  `window-height' -- The value specifies the desired height of the
;;     window chosen and is either an integer (the total height of
;;     the window), a floating point number (the fraction of its
;;     total height with respect to the total height of the frame's
;;     root window) or a function to be called with one argument -
;;     the chosen window.  The function is supposed to adjust the
;;     height of the window; its return value is ignored.  Suitable
;;     functions are `shrink-window-if-larger-than-buffer' and
;;     `fit-window-to-buffer'.
;;  `window-width' -- The value specifies the desired width of the
;;     window chosen and is either an integer (the total width of
;;     the window), a floating point number (the fraction of its
;;     total width with respect to the width of the frame's root
;;     window) or a function to be called with one argument - the
;;     chosen window.  The function is supposed to adjust the width
;;     of the window; its return value is ignored.
;;  `preserve-size' -- The value should be either (t . nil) to
;;     preserve the width of the chosen window, (nil . t) to
;;     preserve its height or (t . t) to preserve its height and
;;     width in future changes of the window configuration.
;;  `window-parameters' -- The value specifies an alist of window
;;     parameters to give the chosen window.
;;  `allow-no-window' -- A non-nil value means that `display-buffer'
;;     may not display the buffer and return nil immediately.

;;; Sources:

;; <https://github.com/karthink/.emacs.d/blob/6aa2e034ce641af60c317697de786bedc2f43a71/lisp/setup-windows.el>

;;; Code:

;;; Requirements

(require 'transient)

(require 'config-buffer)
(require 'config-window)

(require 'lib-common)
(require 'lib-buffer)
(require 'lib-keys)
(require 'lib-window)

;;; General buffer display settings

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

;;; Declare rules for displaying buffers with `display-buffer-alist'

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

;;; Summon and dismiss "popup" windows with `popper'

;; <https://github.com/karthink/popper>

(use-package popper
  :blackout
  :functions (popper-select-popup-at-bottom)

  :preface

  (defun +popper-select-below-fn (buffer &optional _alist)
    (funcall (if (> (frame-width) 170)
               ;; #'display-buffer-in-direction
               #'popper-select-popup-at-bottom
               #'display-buffer-at-bottom)
      buffer
      `((window-height . ,popper-window-height)
         (direction . below)
         (body-function . ,#'select-window))))

  :init

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
      `(,(rx "Output*" eol)
         ,(rx "*" (or
                    "Async-native-compile-log"
                    "Backtrace"
                    "Compile-Log"
                    "Completions"
                    "compilation"

                    "Messages"
                    "Shell Command Output"
                    "vc"
                    "Warnings")
            "*")
         "^\\*Embark Export"
         "^Calc:"
         "\\*Async Shell Command\\*"
         ;; ("\\*Async Shell Command\\*" . hide)
         ("\\*Detached Shell Command\\*" . hide))))

  ;; Load as early as possible to catch popups during startup.
  (popper-mode)
  (popper-echo-mode)

  :config

  (define-keymap :keymap (current-global-map)
    "C-`"   #'popper-toggle
    "C-~"   #'popper-cycle
    "C-M-`" #'popper-toggle-type)
  ;; "M-`"   #'popper-echo-mode

  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards)

  ;; Configure popup display control rules manually.
  ;; <https://github.com/karthink/popper/blob/master/README.org#popup-placement-controlled-using-display-buffer-alist-or-shackleel>
  (setopt popper-display-control nil)

  (prependopt! display-buffer-alist
    '((popper-display-control-p
        (ceamx-window-display-popup)
        (window-height . ,popper-window-height))))

  (after! [projectile]
    (setopt popper-group-function #'popper-group-by-projectile)))

;;; Restore previous window configurations with `winner-mode' [builtin]

(use-feature! winner
  :config (winner-mode))

;;; Add "distraction-free" editing with `olivetti-mode'

;; <https://github.com/rnkn/olivetti>

(use-package olivetti
  :commands (olivetti-mode)

  :init
  (add-hook 'org-mode-hook #'olivetti-mode)

  :config
  (setopt olivetti-style 'fancy))

;;; Interactively manage windows with `ace-window'

;; <https://github.com/abo-abo/ace-window>

(use-package ace-window
  :after (avy)

  :commands (ace-window
              ace-delete-window
              ace-delete-other-windows
              ace-swap-window)

  :autoload (aw-split-window-fair
              aw-split-window-horz
              aw-split-window-vert
              aw-flip-window)

  :config

  ;; Same frame only. While it'd be nice to use the default (global), I really
  ;; dislike that it orders window numbers leads to jarring gaps in window
  ;; numbers in the same frame. For example, frame A might have windows numbered
  ;; 1 and 3 and frame B will have window 2.
  (setopt aw-scope 'frame))

(transient-define-prefix ceamx/window-dispatch ()
  "Window management transient."
  :transient-suffix 'transient--do-stay
  [["Movement"
     ("h" "left" windmove-left :transient t)
     ("j" "down" windmove-down :transient t)
     ("k" "up" windmove-up :transient t)
     ("l" "right" windmove-right :transient t)
     ;; TODO: is this supposed to be a rotator?
     ("o" "other..." aw-flip-window :transient t) ; previously-selected
     ("w" "select..." ace-window)]
    ["Resize"
      ("=" "balance" balance-windows)
      ("+" "balance (area)" balance-windows-area)
      ("-" "fit buffer" fit-window-to-buffer)
      ;; TODO:
      ;; ("<" "width-" evil-window-decrease-width)
      ;; (">" "width+" evil-window-increase-width)
      ;; ("+" "height+" evil-window-increase-height)
      ;; ("-" "height-" evil-window-decrease-height)

      ]
    ["Buffer"
      ("b" "buffer" consult-buffer :transient nil)
      ;; TODO: `ffap-other-window' (doom)
      ("f" "file (p)" project-find-file :transient nil)
      ("F" "file (g)" find-file :transient nil)
      ("F" "file@other" find-file-other-window)
      ("g" "grep" consult-ripgrep :transient nil)]
    ["Swap"
      ("H" "left" windmove-swap-states-left)
      ("J" "down" windmove-swap-states-down)
      ("K" "up" windmove-swap-states-up)
      ("L" "right" windmove-swap-states-right)]
    ["Scroll"
      ;; TODO: allow selecting a window (with infix?) to act upon
      ;; NOTE: These are the correct scroll direction commands, which might
      ;; appear to be reversed when comparing with labels.
      ("." "left" scroll-right)
      ("," "right" scroll-left)
      ("SPC" "down" scroll-up)
      ("DEL" "up" scroll-down)]
    ["Window Lifecycle"
      ("d" "delete" ace-delete-window)
      ("D" "delete others" delete-other-windows :transient nil)
      ("u" "winner ⮐" winner-undo)
      ("U" "winner ⮑" winner-redo)
      ("S" "toggle sides" window-toggle-side-windows)
      ("`" "toggle popups" popper-toggle)

      ("q" "quit" transient-quit-all)]])

;;; Bind keys for window management

(global-keys!
  "C-x o" #'ace-window
  "C-x w" #'ceamx/window-dispatch

  "C-x =" #'balance-windows
  "C-x +" #'balance-windows-area

  "C-x C-n" #'next-buffer
  "C-x C-p" #'previous-buffer

  "C-x <up>" #'enlarge-window           ; also: C-x ^
  "C-x <down>" #'shrink-window
  "C-x <left>" #'shrink-window-horizontally
  "C-x <right>" #'enlarge-window-horizontally)

(keys! resize-window-repeat-map
  "<up>" #'enlarge-window
  "<down>" #'shrink-window
  "<left>" #'shrink-window-horizontally
  "<right>" #'enlarge-window-horizontally)

(provide 'init-window)
;;; init-window.el ends here
