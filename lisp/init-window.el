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

(require 'lib-common)
(require 'lib-window)

;;; Buffer Display

;; Hide buffer until there's output.
;; Prevents an extra window appearing during init.
(setopt async-shell-command-display-buffer nil)

(setopt display-buffer-base-action
  '((display-buffer-reuse-window
      display-buffer-in-previous-window)))
;; FIXME: one of these, i think, is responsible for breaking childframes e.g. `embark-act', `Info-mode'
;; TODO: what do each of these do? doesn't quite make sense...
;; (setopt display-buffer-base-action
;;         '((display-buffer-reuse-mode-window
;;            display-buffer-pop-up-window)
;;           (reusable-frames . t)))

;; TODO: make sure popper configuration is updated accordingly
(setopt display-buffer-alist
  '(
     ("^\\*[Ee]shell [Ee]xport: .*\\*$"
       (display-buffer-reuse-window display-buffer-use-some-window))

;;;; @Top

     ("\\*\\(?:Org Select\\|Agenda Commands\\)\\*"
       (display-buffer-below-selected
         display-buffer-in-side-window)
       (body-function . select-window)
       (window-height . (lambda (win) (fit-window-to-buffer win nil 12)))
       (side . top)
       (slot . -2)
       (preserve-size . (nil . t))
       (window-parameters . ((mode-line-format . nil))))

     ("\\*Buffer List\\*"
       (display-buffer-in-side-window)
       (side . top)
       (slot . 0)
       (window-height . shrink-window-if-larger-than-buffer))

     ((lambda (buf act) (member (ceamx-buffer-mode buf) ceamx-occur-grep-modes-list))
       (display-buffer-reuse-mode-window
         display-buffer-in-direction
         display-buffer-in-side-window)
       (side . top)
       (slot . 5)
       (window-height . (lambda (win) (fit-window-to-buffer win 20 10)))
       (direction . above)
       (body-function . select-window))

     ("\\*\\(Flycheck\\|Package-Lint\\).*"
       (display-buffer-in-direction
         display-buffer-in-side-window)
       (direction . above)
       (window-height . shrink-window-if-larger-than-buffer)
       ;; (window-height . 0.16)
       (side . top)
       (slot . 1)
       (window-parameters . ((no-other-window . t))))

;;;; @Side

;;;; @Bottom

     ((lambda (buf act) (member (ceamx-buffer-mode buf) ceamx-message-modes-list))
       (display-buffer-at-bottom
         display-buffer-in-side-window)
       (window-height . 0.25)
       (side . bottom)
       (slot . -6))

     ("\\*\\(?:Warnings\\|Compile-Log\\|Messages\\)\\*"
       (display-buffer-at-bottom
         display-buffer-in-side-window
         display-buffer-in-direction)
       (window-height . (lambda (win) (fit-window-to-buffer
                                        win
                                        (floor (frame-height) 5))))
       (side . bottom)
       (direction . below)
       (slot . -5)
       (window-parameters . ((split-window . #'ignore)
                              ;; (no-other-window . t)
                              )))

     ("[Oo]utput\\*"
       display-buffer-in-side-window
       (window-height . (lambda (win)
                          (fit-window-to-buffer win (floor (frame-height) 2.5))))
       (side . bottom)
       (slot . -4))

     ("\\*Async Shell Command\\*"
       display-buffer-in-side-window
       (window-height . 0.20)
       (side . bottom)
       (slot . -4)
       (window-parameters . ((no-other-window . t))))

     ;; ("\\*\\(Register Preview\\).*"
     ;;   (display-buffer-in-side-window)
     ;;   (window-height . 0.20)
     ;;   (side . bottom)
     ;;   (slot . -3)
     ;;   (window-parameters . ((no-other-window . t))))

     ("\\*Completions\\*"
       (display-buffer-in-side-window)
       (window-height . 0.20)
       (side . bottom)
       (slot . -2))

     ("\\*Apropos\\*"
       (display-buffer-in-side-window)
       ;; (window-height . 0.40)
       (window-width . 65)
       (side . right)
       (slot . -2))


     ((lambda (buf act) (or (seq-some (lambda (regex) (string-match-p regex buf))
                              ceamx-repl-names-list)
                          (seq-some (lambda (mode)
                                      (equal
                                        (ceamx-buffer-mode buf)
                                        mode))
                            ceamx-repl-modes-list)))
       (display-buffer-reuse-window
         display-buffer-in-direction
         display-buffer-in-side-window)
       (body-function . select-window)
       (window-height . .35)
       (window-width .  .40)
       (direction . below)
       (side . bottom)
       (slot . 1))

     ((lambda (buf act)
        (member (ceamx-buffer-mode buf) ceamx-help-modes-list))
       (display-buffer-reuse-window
         display-buffer-in-direction
         display-buffer-in-side-window)
       (body-function . select-window)
       ;; (direction . bottom)
       ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
       (window-width . 77)
       ;; (window-width . (lambda (win) (fit-window-to-buffer win nil nil 75 65)))
       (direction . below)
       (side . right)
       (slot . 2)
       (window-parameters . ((split-window . #'ignore))))

     ("^\\*eldoc.*\\*$"
       (display-buffer-reuse-window
         display-buffer-in-direction
         display-buffer-in-side-window)
       ;; (body-function . select-window)
       ;; (direction . bottom)
       ;; (window-height . (lambda (win) (fit-window-to-buffer win 25 14)))
       (window-width . 82)
       ;; (window-width . (lambda (win) (fit-window-to-buffer win nil nil 75 65)))
       (direction . below)
       (side . below)
       (slot . 2)
       (window-parameters . ((split-window . #'ignore)
                              (no-other-window . t)
                              (mode-line-format . none))))


     ((lambda (buf act)
        (member (ceamx-buffer-mode buf)
          '(ibuffer-mode bookmark-bmenu-mode)))
       (display-buffer-below-selected)
       (body-function . select-window)
       (direction . below)
       (window-height . (lambda (win) (fit-window-to-buffer win 30 7)))
       ;; (dedicated . t)
       ;; (window-width . (lambda (win) (fit-window-to-buffer win nil nil 85 55)))
       ;; (direction . right)
       (side . bottom)
       (slot . 2))

        ;; ((lambda (buf act) (with-current-buffer buf view-mode))
        ;;  (display-buffer-in-side-window)
        ;;  (window-height . (/ (frame-height) 3))
        ;;  (side . bottom)
        ;;  (slot . 10)
        ;;  ;; (window-parameters . (;; (no-other-window . t)
        ;;  ;;                       ;; (mode-line-format . (:eval (ceamx-helper-window-mode-line-format)))
        ;;  ;;                       ))
        ;;  )

     ))



;; FIXME: not achieving the desired affect
;; via https://github.com/doomemacs/doomemacs/blob/dca4e4a8ed41e0a025d41500297d6fa662d8e22b/modules/ui/popup/%2Bhacks.el>
;; (def-advice! +info-lookup-symbol-focus-window-a (&rest _)
;;   :after #'info-lookup-symbol
;;   "Focus the window opened by `info-lookup-symbol'."
;;   (declare-function popper-popup-p "popper")
;;   (when-let* ((buf (get-buffer "*info*"))
;;                (buf-popup-p (popper-popup-p buf))
;;                (win (get-buffer-window buf)))
;;     (select-window win)))

;; TODO: causes which-key squishing against tiny window maybe?
(setopt fit-window-to-buffer-horizontally t)

;; TODO: this might be a solution to issues with childframes for embark etc.
(setopt fit-frame-to-buffer t)

;; (setopt even-window-sizes nil)
(setopt even-window-sizes 'height-only)
(setopt window-combination-resize t)
(setopt window-sides-vertical nil)
(setopt window-resize-pixelwise t)

;;; Features

(use-feature! winner
  :config (winner-mode))

;;; Packages

;;;; `olivetti-mode' :: <https://github.com/rnkn/olivetti>

(use-package olivetti
  :commands (olivetti-mode)

  :init
  (add-hook 'org-mode-hook #'olivetti-mode)

  :config
  (setopt olivetti-style 'fancy))

;;;; `ace-window' :: <https://github.com/abo-abo/ace-window>

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

  (use-feature! transient
    :config
    ;; via <https://tech.toryanderson.com/2023/08/19/how-can-i-define-transient-color-per-command-like-hydra/>
    (transient-define-prefix ceamx/transient-window ()
      "Window navigation transient."
      :transient-suffix 'transient--do-stay
      [["Movement"
         ;; FIXME: unicode arrows cause space offset
         ;; ("h" "focus ←" windmove-left)
         ;; ("k" "focus ↓" windmove-down)
         ;; ("j" "focus ↑" windmove-up)
         ;; ("l" "focus →" windmove-right)
         ("h" "left" windmove-left)
         ("k" "down" windmove-down)
         ("j" "up" windmove-up)
         ("l" "right" windmove-right)
         ("o" "other" aw-flip-window)   ; previously-selected
         ("w" "select" ace-window)]
        ["Resize"
          ("=" "balance" balance-windows)
          ("-" "match buffer" fit-window-to-buffer)
          ;; ("<" "width-" evil-window-decrease-width)
          ;; (">" "width+" evil-window-increase-width)
          ;; ("+" "height+" evil-window-increase-height)
          ;; ("-" "height-" evil-window-decrease-height)

          ]
        ["Buffer"
          ("b" "buffer" consult-buffer)
          ;; TODO: `ffap-other-window' (doom)
          ("f" "file (p)" project-find-file)
          ("F" "file (g)" find-file :transient transient--do-quit-one)
          ("F" "file@other" find-file-other-window)
          ;; ("F" "follow" follow-mode)
          ("g" "grep" consult-ripgrep :transient transient--do-quit-one)

          ;; ("a" "ace 1" transient-ace-cmd)
          ;; ("&" "sub emacs" tsa/sub-emacs :transient transient--do-quit-one)
          ]
        ["Split"
          ;; TODO: customize direction via infix
          ;; FIXME: cannot eval inside vector -- needs to be a symbol
          ;; ("v" "vertical" (##ceamx/split-window nil 'right))
          ;; ("x" "horizontal" (##ceamx/split-window nil 'below))
          ("H" "to left" ceamx/window-move-left)
          ("J" "to below" ceamx/window-move-down)
          ("K" "to above" ceamx/window-move-up)
          ("L" "to right" ceamx/window-move-right)
          ("s" "swap" ace-swap-window)]
        ["Scroll"
          ("." "left" scroll-left)
          ("," "right" scroll-right)
          ;; ("4" "quad view" tsa/split-window-4)
          ]
        ["Window Lifecycle"
          ("d" "delete" ace-delete-window)
          ;; ("n" "new" evil-window-new)
          ("O" "delete others" ace-delete-other-windows)
          ("q" "quit" transient-quit-all)
          ("z" "winner ⮐" winner-undo)
          ("Z" "winner ⮑" winner-redo)
          ("S" "toggle sides" window-toggle-side-windows)
          ("SPC" "quit" transient-quit-all)]])))

(provide 'init-window)
;;; init-window.el ends here
