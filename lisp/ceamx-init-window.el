;; -*- lexical-binding: t; -*-

(require 'ceamx-simple)
(require 'ceamx-window)

;; Define the user option specifying a fallback buffer


(defcustom ceamx-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist.
The buffer will be created if it does not exist."
  :group 'ceamx
  :type '(string))

;; Configure window behavior for help buffers


;; Focus newly-opened help windows.
(setopt help-window-select t)

;; Also focus newly-opened manpages, which still do not follow `display-buffer'
;; rules (as of <2024-03-06>).
(setopt Man-notify-method 'aggressive)

;; Disambiguate/uniquify buffer names


(use-feature! emacs
  :config
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")

  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)

  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

;; General buffer display settings :buffer:frame:display_buffer:


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



(setopt display-buffer-alist
        `(
          ;; (,(rx "*" (or "Agenda Commands" "Org Select") "*")
          ;;   (display-buffer-below-selected
          ;;     display-buffer-in-side-window)
          ;;   (body-function . select-window)
          ;;   (window-parameters . ((mode-line-format . nil))))

          (,ceamx-simple-checkers-buffer-names-regexp
           (display-buffer-in-direction
            display-buffer-in-side-window)
           (window-parameters . ((no-other-window . t))))

          ;; TODO: is there not a simpler way than using `ceamx-simple-buffer-which-mode'?
          ;; e.g. `derived-mode-p' or similar
          ((lambda (buf act) (member (ceamx-simple-buffer-which-mode buf) ceamx-simple-message-modes-list))
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

;; ~breadcrumb~ :: header-line wayfinding

;; - Package :: <https://github.com/joaotavora/breadcrumb>


(package! breadcrumb
  (add-hook 'ceamx-after-init-hook #'breadcrumb-mode))

;; ~popper~: Summon and dismiss "popup" windows :popups:package:

;; - Website :: <https://github.com/karthink/popper>


(package! popper
  (define-keymap :keymap (current-global-map)
    "C-`" #'popper-toggle
    "C-~" #'popper-cycle
    "C-M-`" #'popper-toggle-type)

  (setopt popper-reference-buffers
          (append
           ceamx-simple-help-modes-list
           ceamx-simple-help-buffer-names-list
           ceamx-simple-manual-modes-list
           ceamx-simple-repl-modes-list
           ceamx-simple-repl-buffer-names-list
           ceamx-simple-grep-modes-list
           '(+popper-current-buffer-popup-p)
           '(Custom-mode
             compilation-mode
             messages-buffer-mode)
           (list
            ceamx-simple-checkers-buffer-names-regexp)

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

;; Configure overrides in ~popper-repeat-map~


(after! popper
  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards))

;; Restore previous window configurations with ~winner-mode~ [builtin] :history:


(add-hook 'ceamx-after-init-hook #'winner-mode)

;; =golden-ratio=: Automatically resize windows according to Ancient Wisdom :package:


(package! golden-ratio
  (setopt golden-ratio-auto-scale t)
  (setopt golden-ratio-max-width 100))

;; =ace-window=: Interactively manage windows :package:

;; <https://github.com/abo-abo/ace-window>


(package! ace-window
  ;; Same frame only. While it'd be nice to use the default (global), I really
  ;; dislike that it orders window numbers leads to jarring gaps in window
  ;; numbers in the same frame. For example, frame A might have windows numbered
  ;; 1 and 3 and frame B will have window 2.
  (setopt aw-scope 'frame))

;; =transpose-frame=: Transpose and rotate a frame's windows :package:


(package! transpose-frame)

;; =lentic=: Create decoupled views of the same content


(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))

;; ~activites~ :: organize window+buffer by activity :package:tabs:

;; - Website :: <https://github.com/alphapapa/activities.el>


(require 'ceamx-lib)

(package! activities
  (activities-mode)

  (when tab-bar-mode
    (activities-tabs-mode)))

;; Activities.el Keybindings :keybinds:


(defun ceamx-after-init-define-activities-keys-h ()
  "Define keybindings for `activities' late to override `edebug'.
Intended for use as a hook on `ceamx-after-init-hook'."
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  ;; (keymap-global-unset "C-x C-a" t)
  (keymap-global-set "C-x C-a" (cons "Activities" (define-prefix-command 'ceamx-activities-prefix)))

  ;; TODO: still shares bindings with edebug which is confusing
  (define-keymap :keymap (current-global-map)
    "C-x C-a C-n" #'activities-new
    "C-x C-a C-d" #'activities-define
    "C-x C-a C-a" #'activities-resume
    "C-x C-a C-s" #'activities-suspend
    "C-x C-a C-k" #'activities-kill
    "C-x C-a RET" #'activities-switch

    "C-x C-a b" #'activities-switch-buffer
    "C-x C-a g" #'activities-revert
    "C-x C-a l" #'activities-list))

(add-hook 'ceamx-after-init-hook #'ceamx-after-init-define-activities-keys-h)

;; ~ceamx/window-dispatch~: a window-management menu :transient:menu:keybinds:


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

(provide 'ceamx-init-window)
;;; ceamx-init-window.el ends here
