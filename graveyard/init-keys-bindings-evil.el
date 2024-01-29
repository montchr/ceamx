;;
;;; Evil Bindings
;;

(after! [evil]
  ;; Don't let evil trample over the immensely-useful question mark.
  (evil-define-key '(normal motion) 'global "?" nil)

  ;;; Normal state
  (define-keymap :keymap evil-normal-state-map
    "f"    #'evil-avy-goto-char-in-line
    "F"    #'avy-goto-char-timer
    "g d"  #'xref-find-definitions
    "g D"  #'xref-find-references
    "g s"  #'avy-goto-char-timer
    "K"    #'helpful-at-point)

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

  ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
  ;; `evil-delete' in wgrep buffers.
  (after! 'wgrep
    (keymap-set wgrep-mode-map "<remap> <evil-delete>" #'+evil-delete))

  (after! [evil-matchit]
    ;; TODO: use `keymap-set'
    (define-key evil-normal-state-map "%" #'evilmi-jump-items)
    (define-key evil-visual-state-map "%" #'evilmi-jump-items)
    (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
    (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object))

  (after! [evil-escape]
    ;; TODO: overlaps with global binding above?
    ;; FIXME: this is prob not what we want... don't we want something like `evil-force-normal-state'?
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

  (after! [evil-args]
    ;; jump out
    ;; FIXME: conflicts with desired K bind
    ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

    ;; inner/outer
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; forward/backward
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)))
