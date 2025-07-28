;; -*- lexical-binding: t; -*-

(require 'ceamx-simple)
(require 'ceamx-window)

;; General customizations
;; :PROPERTIES:
;; :ID:       3edd2008-b6f1-4de2-b0fe-388775c61d93
;; :END:


(setopt split-width-threshold 120
        split-height-threshold nil)

(define-keymap :keymap (current-global-map)
  "C-x =" #'balance-windows
  "C-x +" #'balance-windows-area
  "C-x C-n" #'next-buffer
  "C-x C-p" #'previous-buffer
  ;; TODO: consider sub-mirroring window manager bindings
  "C-x <up>" #'enlarge-window           ; also: C-x ^
  "C-x <down>" #'shrink-window
  "C-x <left>" #'shrink-window-horizontally
  "C-x <right>" #'enlarge-window-horizontally)

(use-feature! ceamx-window
  :bind ("C-x o" . #'ceamx/other-window))

;; Configure window behavior for help buffers


;; Focus newly-opened help windows.
(setopt help-window-select t)

;; Also focus newly-opened manpages, which still do not follow `display-buffer'
;; rules (as of <2024-03-06>).
(setopt Man-notify-method 'aggressive)

;; Disambiguate/uniquify buffer names


(setup uniqify
  (setopt uniquify-buffer-name-style 'forward)
  (setopt uniquify-separator "/")
  ;; Rename after killing uniquified buffer.
  (setopt uniquify-after-kill-buffer-p t)
  ;; Don't muck with special buffers.
  (setopt uniquify-ignore-buffers-re "^\\*"))

;; General buffer display settings :frame:display_buffer:
;; :PROPERTIES:
;; :ID:       20041467-dbd4-484f-9034-41ffca2b15a9
;; :END:


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

;; =popper= :: Summon and dismiss "popup" windows :popup:
;; :PROPERTIES:
;; :ID:       22ada524-f8e6-4f97-b745-5e00219c1be4
;; :END:

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
           '(;; Custom-mode
             compilation-mode
             epa-info-mode
             messages-buffer-mode)
           (list
            ceamx-simple-checkers-buffer-names-regexp)

           `(,(rx "Output*" eol)
             ,(rx "*" (or
                       "Async-native-compile-log"
                       "Backtrace"
                       "Compile-Log"
                       "Completions"
                       "compilation"
                       "elpaca-diff"
                       "Error"
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

  ;; Load as early as possible to catch popups as early as possible.
  (popper-mode)
  (popper-echo-mode))

;; Configure overrides in ~popper-repeat-map~
;; :PROPERTIES:
;; :ID:       df697f37-7a4d-4206-80a2-1814bae3db2b
;; :END:


(after! popper
  (defvar-keymap popper-repeat-map
    :repeat t
    "`" #'popper-cycle
    "~" #'popper-cycle-backwards))

;; =lentic= :: Create decoupled views of the same content
;; :PROPERTIES:
;; :ID:       c6b0beb5-5c7b-4125-b6db-7cf25629a06a
;; :END:


(package! lentic
  (global-lentic-mode))

(with-eval-after-load 'lentic
  (add-to-list 'safe-local-variable-values '(lentic-init . lentic-orgel-org-init)))

;; =breadcrumb= :: header-line wayfinding :headerline:

;; - Package :: <https://github.com/joaotavora/breadcrumb>


(package! breadcrumb
  (add-hook 'ceamx-after-init-hook #'breadcrumb-mode))

;; Restore previous window configurations with the ~winner-mode~ feature :history:


(setup winner
  (:hook-into ceamx-after-init-hook)
  (:with-feature pulsar
    (:when-loaded
      (:option (prepend* pulsar-pulse-functions) '(winner-redo winner-undo)))))

;; =golden-ratio= :: automatically resize windows according to Ancient Wisdom :package:


(package! golden-ratio
  (setopt golden-ratio-auto-scale t)
  (setopt golden-ratio-max-width 100))

;; =ace-window= :: interactively manage windows

;; + Package :: <https://github.com/abo-abo/ace-window>


(setup (:package ace-window)
  (:option aw-scope 'visible)
  (:with-function #'ace-window
    (:bind-to "C-x w w"))
  (:when-loaded
    (:with-feature pulsar
      (:when-loaded
        (:option (prepend* pulsar-pulse-functions)
                 '( aw-copy-window aw-delete-window aw-move-window
                    aw-split-window-fair aw-split-window-horz
                    aw-split-window-vert aw-swap-window))))))

;; =transpose-frame= :: transpose a frame's windows
;; :PROPERTIES:
;; :ID:       76f64487-fbd0-4754-af9b-ceebf4f0916e
;; :END:


(package! transpose-frame
  (keymap-global-set "C-x w SPC" #'transpose-frame))

;; ~ceamx/window-dispatch~: a window-management menu :transient:menu:keybinds:
;; :PROPERTIES:
;; :ID:       5d1605bb-1c26-41cc-a1f7-317354ff113b
;; :END:


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

;; Workspace Isolation :workspace:


(use-feature! ceamx-window
  :demand t
  :config
  (setopt ceamx-window-workspace-provider 'activities))

;; =activities= :: organize window+buffer by activity :tabs:
;; :PROPERTIES:
;; :ID:       91310539-1562-4d0c-9648-0f3aa56cc2f0
;; :END:

;; - Website :: <https://github.com/alphapapa/activities.el>

;; Disabled because =persist= dependency is unavailable, likely due to GNU ELPA fuckery.


(package! activities
  (activities-mode 1)
  (activities-tabs-mode 1)

  ;; Synchronize with future `tab-bar-mode' state changes.
  (add-hook 'tab-bar-mode-hook #'activities-tabs-mode)

  (setopt edebug-inhibit-emacs-lisp-mode-bindings t))

(after! activities
  (setopt activities-name-prefix "α ")
  ;; Don't interfere with the bookmark system.
  (setopt activities-bookmark-store nil)
  ;; Kill buffers upon invocation of `activities-suspend'.
  (setopt activities-kill-buffers t))



;; The keybindings conflict with ~edebug~, so they need to be defined later
;; than usual.  This is still the case as of [2025-01-25 Sat].


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

;; =bufler= :: group buffers with programmable rules
;; :PROPERTIES:
;; :ID:       82ed50ad-45b4-4a0c-9ae5-e978de65cdcd
;; :END:


(package! bufler
  (require 'bufler)

  (define-keymap :keymap (current-global-map)
    ;; "C-x b" #'bufler-switch-buffer
    "C-x B" #'bufler-workspace-focus-buffer
    "C-x C-b" #'bufler                  ; orig. `ibuffer'
    "C-x C-B" #'ibuffer

    "C-x w o" #'bufler-workspace-open
    "C-x w r" #'bufler-workspace-reset
    "C-x w s" #'bufler-workspace-save ; orig. `window-toggle-side-windows'
    )

  (define-keymap :keymap ceamx-workspace-prefix-map
    "TAB" #'bufler-workspace-open
    "b" #'bufler-workspace-switch-buffer
    "B" #'bufler-workspace-focus-buffer
    "r" #'bufler-workspace-reset
    "s" #'bufler-workspace-save))



;; Here are the customizations for Bufler:


(after! bufler
  (setopt bufler-reverse t)
  (setopt bufler-workspace-mode-lighter "β ")
  (setopt bufler-columns '("Name" "Size" "Mode" "VC" "Path"))
  (setopt bufler-initial-face-depth 1)
  (after! prism
    (setopt bufler-face-prefix "prism-level-")))



;; Here are the definitions for the Bufler grouping rules:


(after! bufler
  ;; XXX: This absolutely MUST NOT use `setopt'!  Otherwise, the
  ;; package will fail to load due to a hard-to-trace error saying
  ;; something about nil being a void-function.  This probably has
  ;; something to do with the `setopt' type-checking.
  (setq bufler-groups
        (bufler-defgroups
          (group
           ;; Subgroup collecting all named workspaces.
           (auto-workspace))

          (group (mode-match "Ement" (rx bos "ement-")))
          (group (group-or "Elfeed"
                           (mode-match "*Elfeed*" (rx bos "elfeed-"))
                           (name-match "elfeed config" (rx bos "elfeed." (or "el" "org")))))

          ;;
          ;; Help / info / manuals
          (group
           (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))

          ;;
          ;; Special buffers
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are
           ;; not file-backed), except `magit-status-mode' buffers (which
           ;; are allowed to fall through to other groups, so they end up
           ;; grouped with their project buffers).
           (group-not "*Special*"
                      (group-or "*Special*"
                                (mode-match "Magit" (rx bos "magit-status"))
                                (mode-match "Forge" (rx bos "forge-"))
                                (mode-match "Org" (rx bos "org-"))
                                (auto-file)
                                (mode-match "Dired" (rx bos "dired"))))
           (group
            ;; Subgroup collecting these "special special" buffers
            ;; separately for convenience.
            (name-match "**Special**"
                        (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group
            ;; Subgroup collecting all other Magit buffers, grouped by
            ;; directory.
            (mode-match "*Magit* (non-status)"
                        (rx bos (or "magit" "forge") "-"))
            (auto-directory))
           (auto-mode))

          ;;
          ;; Ceamx
          (group
           (dir user-emacs-directory)
           (auto-parent-project))

          ;;
          ;; Jobwork
          (group
           ;; TODO: make ~/Projects/work a variable `ceamx-jobwork-dir'
           (dir (list (file-name-concat ceamx-projects-dir "work")
                      ;; TODO: make ~/Documents a variable `ceamx-documents-dir'
                      (file-name-concat (alist-get "DOCUMENTS" xdg-user-dirs) "work")))
           (dir '("~/Projects/work/applications") 1)
           (dir ceamx-note-work-dir)
           (group (auto-indirect))
           (auto-parent-project))

          ;;
          ;; Org and notes
          ;; TODO: group journal dir separately for isolation
          (group
           (dir ceamx-agenda-dir)
           (dir ceamx-note-default-dir)
           (dir ceamx-note-journal-dir)
           (group
            ;; Subgroup collecting indirect Org buffers, grouping them by
            ;; file.  This is very useful when used with
            ;; `org-tree-to-indirect-buffer'.
            (auto-indirect)
            (auto-file))
           ;; Group remaining buffers by whether they're file backed, then
           ;; by mode.
           (group-not "*special*" (auto-file))
           (auto-mode))

          (group-or "Home"
                    (dir '("/etc/nixos" "~/.config")))

          (group
           (auto-parent-project)
           (auto-indirect))

          (auto-directory)
          (auto-mode))))

;; Switch buffers with scoped buffer lists


(use-feature! ceamx-window
  :commands (ceamx/switch-to-buffer)
  :bind
  ("C-x b" . #'ceamx/switch-to-buffer))

;; Pulse on window actions with =pulsar= :ui:pulsar:


(setup ceamx-window
  (:with-feature pulsar
    (:when-loaded
      (:option (prepend* pulsar-pulse-functions)
  	       '(ceamx/buffer-create
  	         ceamx/other-window
  	         ceamx/split-window
  	         ceamx/swap-or-rotate-windows
  	         ceamx-window-swap-or-split)))))

(provide 'ceamx-init-window)
;;; ceamx-init-window.el ends here
