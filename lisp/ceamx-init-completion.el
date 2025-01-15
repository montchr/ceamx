;; -*- lexical-binding: t -*-

(require 'ceamx-lib)

;; Baseline completion settings


(use-feature! minibuffer
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (ceamx-after-init . minibuffer-electric-default-mode))

  :config

  (setopt echo-keystrokes 0.25)
  (setopt savehist-save-minibuffer-history t)

  ;; Always resize mini-windows to fit their contents.
  (setopt resize-mini-windows t)
  ;; Hide commands in M-x which do not apply to the current mode.
  (setopt read-extended-command-predicate #'command-completion-default-include-p)

  (setopt minibuffer-prompt-properties
          '(read-only t
            cursor-intangible t         ; see `cursor-intangible-mode'
            face minibuffer-prompt))

  ;; TODO: isn't this handled somewhere else?  case-insensitive until
  ;; proven otherwise?
  ;; (setopt completion-ignore-case t
  ;;         read-buffer-completion-ignore-case t
  ;;         read-file-name-completion-ignore-case t)
  ;; (setq-default case-fold-search t)

  (setopt minibuffer-default-prompt-format " [%s]")

  )

(add-hook 'ceamx-after-init-hook #'minibuffer-depth-indicate-mode)

(after! mb-depth
  (setopt enable-recursive-minibuffers t)
  ;; TODO: evaluate...
  (setopt read-minibuffer-restore-windows nil))

;; Add an indicator to the ~completing-read-multiple~ prompt


(defvar crm-separator)

(def-advice! ceamx-completion-crm-indicator-a (args)
  :filter-args #'completing-read-multiple
  "Add prompt indicator to `completing-read-multiple' for candidates ARGS.
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

;; ~orderless~ :: completion-matching multiple regular expressions :search:

;; + Package :: <https://github.com/oantolin/orderless>


(package! orderless
  (require 'orderless)

  (setopt orderless-matching-styles
          '(orderless-prefixes
            orderless-regexp))

  ;; spaces & dash & slash & underscore
  (setopt orderless-component-separator " +\\|[-/_]")

  ;; [SPC] should never trigger a completion.
  (keymap-set minibuffer-local-completion-map "SPC" nil)
  ;; [?] should not interfere with regexp symbols
  (keymap-set minibuffer-local-completion-map "?" nil))

;; ~+orderless-fast-dispatch~

;; - source :: <https://github.com/minad/corfu/blob/main/README.org#auto-completion>


(defun +orderless-fast-dispatch (word index total)
  "Fast-dispatch `orderless' completion style for `corfu'."
  (and (= index 0) (= total 1) (length< word 4)
       (cons 'orderless-literal-prefix word)))

(after! orderless
  (orderless-define-completion-style +orderless-fast
    "Fast completion style, intended for usage with `corfu'."
    (orderless-style-dispatchers '(+orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))

;; ~+orderless-with-initialism~


(after! orderless
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

;; Configure preferred completion styles


;; Make `partial-completion' behave like `substring'.
(setopt completion-pcm-leading-wildcard t)

;; Reset per-category defaults to force the use of the standard
;; `completion-styles'.  Customizations can be specified in
;; `completion-category-overrides'.
(setq completion-category-defaults nil)

(after! orderless
  (setopt completion-styles '(orderless substring initials flex basic))
  (setopt completion-category-overrides
          '((file (styles basic partial-completion orderless))
            (bookmark (styles basic substring))
            (library (styles basic substring))
            (imenu (styles basic substring orderless))
            (kill-ring (styles emacs22 orderless))
            (eglot (styles emacs22 substring orderless)))))

(after! (consult orderless)
  (add-to-list 'completion-category-overrides
      '(consult-location (styles basic substring orderless))))

(after! embark
  (add-to-list 'completion-category-overrides
      '(embark-keybinding (styles basic substring))))

;; ~vertico~ :: [VERT]ical [I]nteractive [CO]mpletion :minibuffer:

;; + Package :: <https://github.com/minad/vertico>


(package! vertico
  (add-hook 'ceamx-after-init-hook #'vertico-mode)

  (setopt vertico-count 8
          vertico-cycle t
          vertico-resize t
          vertico-scroll-margin 0)

  (after! (vertico savehist)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (add-to-list 'savehist-additional-variables #'vertico-repeat-history))

  ;; Tidy shadowed file names -- e.g. cleans `~/foo/bar///' to `/', and `~/foo/bar/~/' to `~/'.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(after! vertico
  (define-keymap :keymap vertico-map
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    "M-q" #'vertico-quick-insert
    "C-q" #'vertico-quick-exit))



;; Fine-tune Vertico appearance per-command or per-category with
;; ~vertico-multiform~:


(after! vertico
  ;; NOTE: Takes precedence over `vertico-multiform-categories'.
  (setopt vertico-multiform-commands
          `((consult-line buffer)
           (consult-imenu buffer)
           (consult-org-heading ,(lambda (_) (text-scale-set -1)))))

  (setopt vertico-multiform-categories
          '((buffer flat (vertico-cycle . t))
            (consult-grep buffer)
            (imenu (:not indexed mouse))
            (symbol (vertico-sort-function . vertico-sort-alpha))))

  (vertico-multiform-mode))

(after! vertico-multiform
  (keymap-set vertico-multiform-map "C-l" #'vertico-multiform-vertical))

;; ~marginalia~ :: minibuffer completion annotations :minibuffer:

;; + Package :: <https://github.com/minad/marginalia>


(package! marginalia
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

  (marginalia-mode))

;; ~consult~ :: [CONSULT]ing ~completing-read~ :minibuffer:

;; - website :: <https://github.com/minad/consult>
;; - ref :: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html>


(package! consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI.
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  ;; Improve previews for `consult-register' and other register commands
  (setopt register-preview-delay 0.5)
  (setopt register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Display xref locations with previews
  (setopt xref-show-definitions-function #'consult-xref)
  (setopt xref-show-xrefs-function #'consult-xref))

(after! consult
  (require 'consult-imenu)

  (setopt consult-narrow-key "<")       ; alternative: "C-+"
  (setopt consult-line-numbers-widen t)
  (setopt consult-async-min-input 3
          consult-async-input-debounce 0.5
          consult-async-input-throttle 0.8)

  (after! pulsar
    (setq consult-after-jump-hook nil)
    (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn))))



;; Refine preview appearance and behavior:


(after! consult
  (setopt consult-preview-key 'any)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)

   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any)))



;; Preview files in =find-file=:

;; - source :: <https://github.com/minad/consult/wiki#previewing-files-in-find-file>


(use-feature! ceamx-completion
  :demand t
  :after consult
  :commands (ceamx-completion/consult-find-file-with-preview)
  :init
  (setq read-file-name-function #'ceamx-completion/consult-find-file-with-preview))



;; Define commands to search pre-defined sets of Info pages with
;; ~consult-info~:


(define-prefix-command 'ceamx-info-prefix 'ceamx-info-prefix-map)

(keymap-global-set "C-h i" (cons "[info]" #'ceamx-info-prefix))

(use-feature! ceamx-completion
  :after consult
  :commands (ceamx/consult-info-dwim
             ceamx/completion-info
             ceamx/emacs-info
             ceamx/org-info)
  :init
  (define-keymap :keymap help-map
    "i i" #'ceamx/consult-info-dwim
    "i c" #'ceamx/completion-info
    "i e" #'ceamx/emacs-info
    "i o" #'ceamx/org-info))

;; Define keybindings for ~consult~ and its extensions :keybinds:


(define-keymap :keymap (current-global-map)
  "C-c M-x" #'consult-mode-command

  "<remap> <Info-search>" #'consult-info

  "C-x M-:" #'consult-complex-command ; orig. `repeat-complex-command'
  "C-x b" #'consult-buffer            ; orig. `switch-to-buffer'
  "C-x 4 b" #'consult-buffer-other-window ; orig. `switch-to-buffer-other-window'
  "C-x 5 b" #'consult-buffer-other-frame ; orig. `switch-to-buffer-other-frame'
  "C-x t b" #'consult-buffer-other-tab ; orig. `switch-to-buffer-other-tab'
  "C-x r b" #'consult-bookmark         ; orig. `bookmark-jump'
  "C-x p b" #'consult-project-buffer ; orig. `project-switch-to-buffer'

  ;; [C-h] bindings (`help-map')
  "C-h I" #'consult-info ; orig. `describe-input-method'

  ;; Custom M-# bindings for fast register access
  "M-#"    #'consult-register-load
  "M-'"    #'consult-register-store ; orig. `abbrev-prefix-mark' (unrelated)
  "C-M-#"  #'consult-register

  ;; TODO: reconcile with current binding for `forward-symbol'
  ;; "M-F" #'consult-focus-lines
  "M-K" #'consult-keep-lines
  "M-y" #'consult-yank-pop              ; orig. `yank-pop'

  ;; M-g bindings (`goto-map')
  "M-g e"  #'consult-compile-error
  "M-g f"  #'consult-flymake            ; or: `consult-flycheck'
  "M-g g"  #'consult-goto-line          ; orig. `goto-line'
  "M-g M-g" #'consult-goto-line         ; orig. `goto-line'
  "M-g o"  #'consult-outline            ; or: `consult-org-heading'
  "M-g m"  #'consult-mark
  "M-g k"  #'consult-global-mark
  "M-g i"  #'consult-imenu
  "M-g I"  #'consult-imenu-multi

  ;; M-s bindings (`search-map')
  "M-s d"  #'consult-fd                 ; or `consult-find'
  "M-s c"  #'consult-locate
  "M-s e"  #'consult-isearch-history
  "M-s g"  #'consult-ripgrep
  "M-s G"  #'consult-git-grep
  "M-s k"  #'consult-keep-lines
  "M-s l"  #'consult-line
  "M-s L"  #'consult-line-multi
  "M-s u"  #'consult-focus-lines
  "M-s M-s" #'consult-outline)

(after! isearch
  (define-keymap :keymap isearch-mode-map
    "M-e"   #'consult-isearch-history   ; orig. `isearch-edit-string'
    "M-s e" #'consult-isearch-history   ; orig. `isearch-edit-string'
    "M-s l" #'consult-line              ; needed by `consult-line' to detect `isearch'
    "M-s L" #'consult-line-multi        ; needed by `consult-line' to detect `isearch'
    ))

(keymap-set minibuffer-local-map "M-s" #'consult-history) ; orig. `next-matching-history-element'
(keymap-set minibuffer-local-map "M-r" #'consult-history) ; orig. `previous-matching-history-element'

(after! consult
  ;; Make narrowing help available in the minibuffer.
  (define-keymap :keymap consult-narrow-map
    "?" #'consult-narrow-help)
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'embark-prefix-help-command))

;; Dynamic text expansion with ~dabbrev~


(setopt dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
        dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
(setopt dabbrev-backward-only nil)
(setopt dabbrev-case-distinction 'case-replace
        dabbrev-case-fold-search nil
        dabbrev-upcase-means-case-search t)
(setopt dabbrev-check-other-buffers t)
(setopt dabbrev-eliminate-newlines t)
(setopt dabbrev-ignored-buffer-regexps '("\\` ")
        dabbrev-ignored-buffer-modes '(archive-mode doc-view-mode image-mode
                                       pdf-view-mode tags-table-mode))

;; Configure ~abbrev-mode~ settings


(use-feature! abbrev
  ;; NOTE: `message-mode' derives from `text-mode', so it does not
  ;; need to be listed for coverage here.
  :hook ((text-mode prog-mode git-commit-mode) . abbrev-mode)

  :config
  (setopt only-global-abbrevs nil)
  (setopt abbrev-suggest t)

  ;; Our abbrevs are defined in configuration, no need to save elsewhere
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

  ;; Because the *scratch* buffer is produced before we load this, we
  ;; have to explicitly activate the mode there.
  (when-let* ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1))))

;; Define the custom ~abbrevs~


(abbrevs! text-mode-abbrev-table
  "javascript"		"JavaScript"
  "typescript"		"TypeScript"
  "wordpress"		"WordPress"
  "youtube"		"YouTube")

;; ~corfu~ :: [CO]mpletion in [R]egion [FU]nction

;; + Package :: <https://github.com/minad/corfu>
;; + Reference :: <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>


(package! corfu
  (add-hook 'ceamx-after-init-hook #'global-corfu-mode))

(after! corfu
  (setopt corfu-count 12
          corfu-cycle t
          corfu-max-width 80
          corfu-min-width 20
          corfu-scroll-margin 3)
  ;; cf. `orderless-component-separator'
  (setopt corfu-separator ?_)
  (setopt corfu-on-exact-match 'insert
          corfu-preselect 'prompt
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match t)
  (setopt corfu-echo-delay '(0.3 . 0.3))
  (setopt corfu-popupinfo-delay '(1.25 . 0.5))
  (setopt corfu-auto t
          corfu-auto-delay 0.3
          corfu-auto-prefix 3)

  ;; Setting this here again for good measure, just in case it is
  ;; changed elsewhere.
  (setopt tab-always-indent 'complete)

  ;; Prevent excessive completion-spamming.
  ;; Without this, on Emacs 30.0, typing causes constant `corfu' errors.
  ;; <https://github.com/minad/corfu/discussions/457>
  (setopt text-mode-ispell-word-completion nil)

  (keymap-set corfu-map "M-SPC" #'corfu-insert-separator)

  ;; NOTE: Requires `tab-always-indent' to be set to `complete',
  ;; otherwise TAB will *never* indent!
  (keymap-set corfu-map "TAB" #'corfu-complete)

  (corfu-popupinfo-mode 1)
  (corfu-echo-mode -1)

  ;; Sort candidates by input history.
  (after! savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; FIXME: `ceamx-completion-corfu-minibuffer-enable-p' not a function
;; maybe?  type mismatch
;; (use-feature! ceamx-completion
;;   :after corfu
;;   :functions (ceamx-completion-corfu-minibuffer-enable-p)
;;   :init
;;   (setopt global-corfu-minibuffer #'ceamx-completion-corfu-minibuffer-enable-p))

;; ~corfu-terminal~ :: Corfu terminal support

;; + Package :: <https://codeberg.org/akib/emacs-corfu-terminal>

;; Corfu-endorsed solution to making it usable in terminal.

;; See also ~popon~, the utility library powering the interface.


(package! corfu-terminal
  (after! (corfu)
    (unless (display-graphic-p)
      (corfu-terminal-mode 1))))

;; ~kind-icon~ :: icons for ~completion-at-point~ candidates :icons:

;; + Package :: <https://github.com/jdtsmith/kind-icon>


(package! kind-icon
  (require 'kind-icon)

  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-blend-background t)

  (after! corfu
    (setopt kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;; Reduce icon size slightly

;; If you change this value, make sure to call ~kind-icon-reset-cache~ afterwards,
;; otherwise the icon size will likely not be accurate.


(after! kind-icon
  (plist-put kind-icon-default-style :height 0.9))

;; Update icon apperance after enabling a theme


(after! kind-icon
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'ceamx-after-enable-theme-hook #'kind-icon-reset-cache))

;; ~nerd-icons-completion~ :: icons for minibuffer completions :icons:minibuffer:


(package! nerd-icons-completion
  (after! marginalia
    (nerd-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

;; Add command to export completion candidates to a writable buffer


(use-feature! ceamx-completion
  :after minibuffer
  :commands (ceamx-completion/embark-export-write)
  :init
  (keymap-set minibuffer-local-map "C-c C-e" #'ceamx-completion/embark-export-write))

;; ~embark~ :: [E]macs [M]ini-[B]uffer [A]ctions [R]ooted in [K]eymaps

;; - Package :: <https://github.com/oantolin/embark>


(package! embark
  ;; Embark is a heavy package.  Load in the background to avoid
  ;; delays upon invoking autoloaded commands.
  (defer! 3
    (require 'embark))

  ;; NOTE: This key might be bound to emoji input in GNOME Desktop.
  ;; However, I have not encountered a conflict on GNOME, so I must be
  ;; doing something conveniently correct in my GNOME configurations.
  ;; FWIW, I have enabled the Emacs-style keybindings there.
  (keymap-global-set "C-." #'embark-act)

  ;; The result of calling `embark-dwim' on a symbol still ends up
  ;; calling `xref-find-definitions' as the default do-what-i-mean
  ;; action.
  (keymap-global-set "M-." #'embark-dwim) ; orig. `xref-find-definitions'

  (keymap-global-set "C-h b" #'embark-bindings) ; orig: `describe-bindings'
  (keymap-global-set "C-h B" #'describe-bindings)

  (unless (bound-and-true-p which-key-mode)
    (setopt prefix-help-command #'embark-prefix-help-command)))

(after! embark
  (setopt embark-indicators '(;; embark--vertico-indicator
                              ;; embark-mixed-indicator
                              embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))
  (setopt embark-mixed-indicator-delay 2.0)

  ;; You can reverse the configured behavior at any time by calling `embark-act'
  ;; with a "C-u" prefix argument.
  ;; For finer control, e.g.: `((kill-buffer . t) (t . nil))'
  ;; This only affects the behavior of `embark-act' inside the minibuffer.
  (setopt embark-quit-after-action nil)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
      '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
        nil
        (window-parameters (mode-line-format . none)))))

(after! vertico
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(package! embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))



;; Define Embark actions for acting on tabs:


(after! embark
  (defvar-keymap ceamx-embark-tab-actions
    :doc "Keymap for Embark actions for `tab-bar' tabs (when mentioned by name)."
    :parent embark-general-map

    "s" #'tab-bar-select-tab-by-name
    "d" #'tab-bar-close-tab-by-name
    "R" #'tab-bar-rename-tab-by-name)

  (add-to-list 'embark-keymap-alist '(tab . ceamx-embark-tab-actions))

  (push #'embark--confirm
        (alist-get 'tab-bar-close-tab-by-name
                   embark-pre-action-hooks)))

;; Provide feature ~ceamx-init-completion~


(provide 'ceamx-init-completion)
;;; ceamx-init-completion.el ends here
