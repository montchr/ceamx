;; Requirements


(require 'ceamx-lib)
(require 'lib-completion)

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
  (after! minibuffer
    (require 'orderless))

  (setopt orderless-matching-styles '(orderless-prefixes orderless-regexp))
  ;; Spaces & dash & slash & underscore
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


;; See also `completion-category-overrides'
(setopt completion-styles
        '(basic
          substring
          initials
          flex
          orderless))

;; Make `partial-completion' behave like `substring'.
(setopt completion-pcm-leading-wildcard t)

;; Reset per-category defaults to force the use of the standard
;; `completion-styles'.  Customizations can be specified in
;; `completion-category-overrides'.
(setq completion-category-defaults nil)

(setopt completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles .  (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless)))))

;; ~vertico~ :: VERT(ical )I(nteractive )CO(mpletion)

;; + Package :: <https://github.com/minad/vertico>


(package! vertico
  (add-hook 'ceamx-after-init-hook #'vertico-mode)

  (setopt vertico-cycle t)
  (setopt vertico-count 5
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

;; Install and require the Consult package


(package! consult
  (require 'consult))

;; Narrow/filter Consult candidates


(setopt consult-narrow-key ">")         ; suggested: "<"

;; Configure the display of candidate previews :ui:completions:


(setopt consult-preview-key 'any)

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)



;; In addition to executing the standard mode hooks and hooks on =find-file-hook=,
;; allow some light font-locking hooks:


(with-eval-after-load 'consult
  (dolist (hook '(;; local modes on prog-mode hooks
                  hl-todo-mode
                  elide-head-mode
                  ;; enabled global modes
                  global-org-modern-mode
                  global-hl-todo-mode))
    (add-to-list 'consult-preview-allowed-hooks hook)))



;; Granularly refine per-command preview behavior:


(after! consult
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any)))

;; Improve previews for ~consult-register~ and other register commands :registers:


(setopt register-preview-delay 0.5)
(setopt register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)

;; Display xref locations with previews :xref:


(setopt xref-show-definitions-function #'consult-xref)
(setopt xref-show-xrefs-function #'consult-xref)

;; Preview files in =find-file=

;; - source :: <https://github.com/minad/consult/wiki#previewing-files-in-find-file>


(defun +consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))

(after! consult
  (setq read-file-name-function #'+consult-find-file-with-preview))

;; Pulse line for visual feedback upon Consult selection jump :ui:navigation:

;; + [ ] Why after ~consult-imenu~?


(after! (consult consult-imenu pulsar)
  (setq consult-after-jump-hook nil)
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))

;; Define global keybindings for Consult commands


(define-keymap :keymap (current-global-map)
  "C-c M-x" #'consult-mode-command

  "<remap> <Info-search>" #'consult-info

  "C-x M-:" #'consult-complex-command     ; orig. `repeat-complex-command'
  "C-x b" #'consult-buffer                ; orig. `switch-to-buffer'
  "C-x 4 b" #'consult-buffer-other-window ; orig. `switch-to-buffer-other-window'
  "C-x 5 b" #'consult-buffer-other-frame  ; orig. `switch-to-buffer-other-frame'
  "C-x t b" #'consult-buffer-other-tab    ; orig. `switch-to-buffer-other-tab'
  "C-x r b" #'consult-bookmark            ; orig. `bookmark-jump'
  "C-x p b" #'consult-project-buffer      ; orig. `project-switch-to-buffer'

  ;; Custom M-# bindings for fast register access
  "M-#"    #'consult-register-load
  "M-'"    #'consult-register-store     ; orig. `abbrev-prefix-mark' (unrelated)
  "C-M-#"  #'consult-register

  ;; Other custom bindings
  "M-y" #'consult-yank-pop              ; orig. `yank-pop'

  ;; M-g bindings (goto-map)
  "M-g e"  #'consult-compile-error
  "M-g f"  #'consult-flymake            ; or: `consult-flycheck'
  "M-g g"  #'consult-goto-line          ; orig. `goto-line'
  "M-g M-g" #'consult-goto-line         ; orig. `goto-line'
  "M-g o"  #'consult-outline            ; or: `consult-org-heading'
  "M-g m"  #'consult-mark
  "M-g k"  #'consult-global-mark
  "M-g i"  #'consult-imenu
  "M-g I"  #'consult-imenu-multi

  ;; M-s bindings (search-map)
  "M-s d"  #'consult-fd                 ; or `consult-find'
  "M-s c"  #'consult-locate
  "M-s e"  #'consult-isearch-history
  "M-s g"  #'consult-ripgrep
  "M-s G"  #'consult-git-grep
  "M-s l"  #'consult-line
  "M-s L"  #'consult-line-multi
  "M-s k"  #'consult-keep-lines
  "M-s u"  #'consult-focus-lines)

;; Define keybindings in ~isearch-mode-map~ for ~consult-line~ integration :isearch:


(after! isearch
  (define-keymap :keymap isearch-mode-map
    "M-e"   #'consult-isearch-history   ; orig. `isearch-edit-string'
    "M-s e" #'consult-isearch-history   ; orig. `isearch-edit-string'
    "M-s l" #'consult-line              ; needed by `consult-line' to detect `isearch'
    "M-s L" #'consult-line-multi        ; needed by `consult-line' to detect `isearch'
    ))

;; Define minibuffer-local keybindings for searching its history :minibuffer:history:


(keymap-set minibuffer-local-map "M-s" #'consult-history) ; orig. `next-matching-history-element'
(keymap-set minibuffer-local-map "M-r" #'consult-history) ; orig. `previous-matching-history-element'

(after! consult
  ;; Make narrowing help available in the minibuffer.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'embark-prefix-help-command))

;; Pre-defined filters for ~consult-info~ searches


(require 'ceamx-lib)

(defvar devdocs-data-dir)

(declare-function consult-info "consult")

;; via <https://github.com/minad/consult?tab=readme-ov-file#help>
(defun ceamx/emacs-info ()
  "Search through Emacs info pages."
  (interactive)
  (consult-info "emacs" "efaq" "elisp" "cl"))

(defun ceamx/org-info ()
  "Search through the Org info page."
  (interactive)
  (consult-info "org"))

(defun ceamx/completion-info ()
  "Search through completion info pages."
  (interactive)
  (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                "corfu" "cape" "tempel"))

(defun ceamx/consult-info-dwim (&optional buffer)
  "Search Info manuals appropriate to BUFFER's major-mode."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((mode major-mode)
           (fn (pcase mode
                 ((pred (lambda (x) (memq x '(emacs-lisp-mode))))
                  #'ceamx/emacs-info)
                 ((pred (lambda (x) (memq x '(org-mode org-agenda-mode))))
                  #'ceamx/org-info)
                 (_ #'consult-info))))
      (command-execute fn))))

;; Bind commands to call ~consult-info~ filtered by commonly-used manual collections

;; Refile... somewhere...


(declare-function consult-info "consult-info")

;; Remove the default binding for the `describe-input-method' command.
(keymap-global-unset "C-h I" t)

(define-keymap :keymap (current-global-map)
  "C-h i"    #'ceamx/consult-info-dwim
  "C-h I c"  #'ceamx/completion-info
  "C-h I e"  #'ceamx/emacs-info
  "C-h I i"  #'consult-info
  "C-h I o"  #'ceamx/org-info)

;; ~marginalia~ :: minibuffer completion annotations

;; + Package :: <https://github.com/minad/marginalia>


(package! marginalia
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

  (marginalia-mode))

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

;; DISABLED Allow abbrevs with a prefix colon, semicolon, or underscore

;; Disabled to test interference with other completion-at-point behavior wired
;; together through [[*Completion-At-Point Extensions (Cape)]].

;; - Source :: <https://github.com/protesilaos/dotfiles/blob/8fc72724cd6debd12c8258bf64adf6822a0bc90c/emacs/.emacs.d/prot-emacs-modules/prot-emacs-completion.el#L215-L225>
;; - Background :: <https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/>

;; Adapted from Prot's original version with the following changes:

;; - Converted the duplicated regexp string into an ~rx~ form via the ~xr~ utility.
;; - Abstracted the regexp to a variable ~ceamx-abbrev-prefix-regexp~ for reuse
;;   across =abbrev-table= contexts.


(defconst ceamx-abbrev-prefix-regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:_].*\\|.*\\)")

(after! abbrev
  (abbrev-table-put global-abbrev-table :regexp ceamx-abbrev-prefix-regexp)

  (with-eval-after-load 'text-mode
    (abbrev-table-put text-mode-abbrev-table :regexp ceamx-abbrev-prefix-regexp))

  (with-eval-after-load 'org
    (abbrev-table-put org-mode-abbrev-table :regexp ceamx-abbrev-prefix-regexp)))

;; ~corfu~ :: CO(mpletion in )R(egion )FU(nction)

;; + Package :: <https://github.com/minad/corfu>
;; + Reference :: <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>


(package! corfu
  (add-hook 'ceamx-after-init-hook #'global-corfu-mode)

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

  (after! corfu
    (keymap-set corfu-map "M-SPC" #'corfu-insert-separator)

    ;; NOTE: Requires `tab-always-indent' to be set to `complete',
    ;; otherwise TAB will *never* indent!
    (keymap-set corfu-map "TAB" #'corfu-complete)

    (unless corfu-popupinfo-mode
      (corfu-echo-mode 1)))

  (after! (corfu savehist)
    (corfu-history-mode 1)

    (add-to-list 'savehist-additional-variables 'corfu-history)))



;; Conditionally enable/disable Corfu in minibuffers


(setopt global-corfu-minibuffer #'ceamx-completion-corfu-minibuffer-enable-p)

(defun ceamx-completion-corfu-minibuffer-enable-p ()
  "Whether to enable `corfu' completion in a currently-active minibuffer."
  (not (or (bound-and-true-p mct--active)
           (bound-and-true-p vertico--input)
           (eq (current-local-map) read-passwd-map))))

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

;; Add command to export completion candidates to a writable buffer


(keymap-set minibuffer-local-map "C-c C-e" #'ceamx-completion/embark-export-write)



;; Note the dependencies on ~embark~ and ~wgrep~!


;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/completion/vertico/autoload/vertico.el#L91-L108>
;;;###autoload
(defun ceamx-completion/embark-export-write ()
  "Export the current `vertico' candidates to a writable buffer.
Supported export flows include the following:

`consult-grep'      => `wgrep'
files               => `wdired'
`consult-location'  => `occur-edit'"
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'occur-edit-mode)
              (x (user-error "Embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (embark-export)))

;; Provide feature ~ceamx-init-completion~


(provide 'ceamx-init-completion)
;;; ceamx-init-completion.el ends here
