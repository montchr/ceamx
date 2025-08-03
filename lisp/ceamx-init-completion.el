;; -*- lexical-binding: t -*-

(require 'ceamx-lib)

;; Baseline minibuffer settings
;; :PROPERTIES:
;; :ID:       2c5efde3-41fb-4154-bbc9-3921ad9a318b
;; :END:


(use-feature! emacs
  :hook ((ceamx-after-init . (minibuffer-depth-indicate-mode
                              minibuffer-electric-default-mode)))
  :config

  (setopt echo-keystrokes 0.25)
  (setopt savehist-save-minibuffer-history t)

  ;; Allow opening the minibuffer from inside the minibuffer.
  (setopt enable-recursive-minibuffers t)
  ;; Expand mini-windows to fit their contents if necessary.
  (setopt resize-mini-windows 'grow-only)
  ;; Hide commands in M-x which do not apply to the current mode.
  (setopt read-extended-command-predicate #'command-completion-default-include-p)
  ;; Hide the cursor in the minibuffer prompt.
  (setopt minibuffer-prompt-properties '( read-only t
                                          cursor-intangible t
                                          face minibuffer-prompt))

  (setopt completion-ignore-case t
          read-buffer-completion-ignore-case t
          read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)

  (setopt minibuffer-default-prompt-format " [%s]"))

;; Add an indicator to the ~completing-read-multiple~ prompt
;; :PROPERTIES:
;; :ID:       6d74806b-359a-45e8-9090-b5b88640c900
;; :END:


;; Supported out of the box in Emacs 31 with `crm-prompt'.
(when (< emacs-major-version 31)
  (def-advice! ceamx-completion-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    "Add prompt indicator to `completing-read-multiple' for candidates ARGS.
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
                  (string-replace "[ \t]*" "" crm-separator)
                  (car args))
          (cdr args))))

;; =vertico= :: [vert]ical [i]nteractive [co]mpletion
;; :PROPERTIES:
;; :ID:       4d5b4204-7e8e-4b11-87d1-5da6618f99ec
;; :END:

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
    ;; TODO: prevent adding deletion to kill-ring
    "M-DEL" #'vertico-directory-delete-word
    "M-q" #'vertico-quick-insert
    "C-j" #'vertico-insert
    "C-q" #'vertico-quick-exit))

(use-feature! ceamx-completion
  :after vertico
  :bind ( :map vertico-map
          ;; ("TAB" . ceamx/vertico-partial-insert)
          ("C-j" . vertico-insert)))



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



;; Provide a way to change the sort order inside an active Vertico
;; session:


(use-feature! ceamx-completion
  :after vertico
  :bind ( :map vertico-map
          ("M-l" . ceamx/vertico-session-sort-normal)
          ("M-L" . ceamx/vertico-session-sort-alpha)))

;; =marginalia= :: minibuffer completion annotations

;; + Package :: <https://github.com/minad/marginalia>


(package! marginalia
  (keymap-set minibuffer-local-map "M-a" #'marginalia-cycle)

  (marginalia-mode 1))

(after! marginalia
  (setopt marginalia-align 'right
          marginalia-align-offset 0))

;; =nerd-icons-completion= :: icons for minibuffer completions :icons:


(package! nerd-icons-completion
  (after! marginalia
    (nerd-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

;; =consult= :: [consult]ing ~completing-read~
;; :PROPERTIES:
;; :ID:       2eff5461-19e2-49a8-9c43-17b2dd6d76f7
;; :END:

;; - website :: <https://github.com/minad/consult>
;; - ref :: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html>


(package! consult
  (keymap-global-set "C-c b b" #'consult-bookmark)
  (keymap-global-set "C-c h" #'consult-history)
  (keymap-global-set "C-c k" #'consult-kmacro)
  (keymap-global-set "C-c q a t" #'consult-theme)

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

;; Search pre-defined sets of Info pages with ~consult-info~


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

  ;; "C-x b" #'consult-buffer              ; orig. `switch-to-buffer'

  "C-x 4 b" #'consult-buffer-other-window ; orig. `switch-to-buffer-other-window'
  "C-x 5 b" #'consult-buffer-other-frame ; orig. `switch-to-buffer-other-frame'
  "C-x t b" #'consult-buffer-other-tab ; orig. `switch-to-buffer-other-tab'
  "C-x r b" #'consult-bookmark         ; orig. `bookmark-jump'
  "C-x p b" #'consult-project-buffer ; orig. `project-switch-to-buffer'

  ;; [C-h] bindings (`help-map')
  "C-h I" #'consult-info               ; orig. `describe-input-method'

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

;; =orderless= :: completion-matching multiple regular expressions :search:
;; :PROPERTIES:
;; :ID:       d30dd746-1706-4c46-901d-47a247455fca
;; :END:

;; + Package :: <https://github.com/oantolin/orderless>


(package! orderless
  (require 'orderless)

  (setopt orderless-matching-styles
          '(orderless-prefixes
            orderless-regexp))

  (setopt orderless-component-separator #'orderless-escapable-split-on-space)
  ;; spaces & dash & slash & underscore
  ;; (setopt orderless-component-separator " +\\|[-/_]")

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
;; :PROPERTIES:
;; :ID:       f2d291ff-f65d-431d-8714-0cc747c872f6
;; :END:


(after! orderless
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))

;; Configure preferred completion styles
;; :PROPERTIES:
;; :ID:       1c08e88d-5386-4929-bac7-9e679dfbc6e6
;; :END:


;; Make `partial-completion' behave like `substring'.
(setopt completion-pcm-leading-wildcard t)

(after! orderless
  ;; Reset per-category defaults to force the use of the standard
  ;; `completion-styles'.  Customizations can be specified in
  ;; `completion-category-overrides'.
  (setq completion-category-defaults nil)
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-overrides
          '((file (styles basic partial-completion))
            (bookmark (styles basic substring))
            (library (styles basic substring))
            (imenu (styles orderless substring basic))
            (kill-ring (styles emacs22 orderless))
            ;; enable initialism by default for symbols
            (command (styles +orderless-with-initialism))
            (variable (styles +orderless-with-initialism))
            (symbol (styles +orderless-with-initialism)))))

(after! (consult orderless)
  (add-to-list 'completion-category-overrides
      '(consult-location (styles basic substring orderless))))

(after! eglot
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))
  ;; FIXME: who provides `eglot-capf'?
  (add-to-list 'completion-category-overrides '(eglot-capf (styles orderless))))

(after! embark
  (add-to-list 'completion-category-overrides
      '(embark-keybinding (styles basic substring))))

(after! (orderless consult)
  (require 'ceamx-completion)

  (setopt orderless-style-dispatchers
          '(ceamx-completion-orderless-consult-dispatch
            orderless-affix-dispatch)))

;; Dynamic text expansion with ~dabbrev~


(after! dabbrev
  ;; (setopt dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
  ;;         dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setopt dabbrev-backward-only nil)
  (setopt dabbrev-case-distinction 'case-replace
          dabbrev-case-fold-search nil
          dabbrev-upcase-means-case-search t)
  (setopt dabbrev-check-other-buffers t)
  (setopt dabbrev-eliminate-newlines t)
  (setopt dabbrev-ignored-buffer-regexps '("\\` ")
          dabbrev-ignored-buffer-modes '( archive-mode doc-view-mode image-mode
                                          pdf-view-mode tags-table-mode)))

;; Static text expansion with ~abbrev~
;; :PROPERTIES:
;; :ID:       0076b762-4975-4973-8326-f413860fa3c4
;; :END:


(use-feature! abbrev
  :hook ((text-mode prog-mode) . abbrev-mode)

  :config
  (setopt only-global-abbrevs nil)
  (setopt abbrev-suggest t)

  ;; Our abbrevs are defined in configuration, no need to save elsewhere
  ;; (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

  ;; Because the *scratch* buffer is produced before we load this, we
  ;; have to explicitly activate the mode there.
  (when-let* ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1))))



;; Define the custom abbrevs:


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
  (after! text-mode
    (abbrev-table-put text-mode-abbrev-table :regexp ceamx-abbrev-prefix-regexp))
  (after! org
    (abbrev-table-put org-mode-abbrev-table :regexp ceamx-abbrev-prefix-regexp)))

;; Configuration


(package! tempel
  (define-keymap :keymap (current-global-map)
    "C-c y" #'tempel-insert

    "M-+" #'tempel-complete
    "M-*" #'tempel-insert)

  ;; Setup completion at point for Tempel templates.
  ;;
  ;; FIXME: prevent automatically completing `tempel-trigger-prefix'
  ;; when no other completion candidates are available.  This can
  ;; occur when: 1. invoking `completion-at-point' directly via M-x;
  ;; 2. via a custom keybinding for `completion-at-point'; 3. when
  ;; typing the literal "<" character and `corfu-auto' is non-nil
  (def-hook! +tempel-setup-capf-h ()
    '(conf-mode-hook prog-mode-hook text-mode-hook)
    "Add the Tempel Capf to `completion-at-point-functions'.

`tempel-expand' only triggers on exact matches.  Alternatively
use `tempel-complete' if you want to see all matches, but then
you should also configure `tempel-trigger-prefix', such that
Tempel does not trigger too often when you don't expect it."
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t)))

(after! tempel
  (setopt tempel-path (file-name-concat ceamx-templates-dir "tempel/*.eld"))

  ;; Require this prefix before triggering template name completion.
  (setopt tempel-trigger-prefix "<")

  (define-keymap :keymap tempel-map
    ;; "<tab>" #'tempel-next
    ;; "<backtab>" #'tempel-previous
    "M-}" #'tempel-next
    "M-{" #'tempel-previous))

(use-feature! ceamx-completion
  :after tempel
  :functions (ceamx-completion--tempel-include)
  :init
  (add-to-list 'tempel-user-elements #'ceamx-completion--tempel-include))

;; =yasnippet= :: robust template expansions :lsp:

;; - Documentation :: <https://github.com/joaotavora/yasnippet/blob/master/README.mdown>
;; - Website :: <https://joaotavora.github.io/yasnippet/>

;; - Easy to convert from TextMate snippet syntax, making its largely
;;   interoperable with a wider ecosystem.
;; - Supports nested placeholders, which in my opinion is an essential
;;   feature for a snippet syntax.  While Tempel also can support this, the
;;   feature must be custom, and the example provided in its documentation
;;   does not handle multiple levels of nesting well.
;; - Yasnippet is /required/ for some types of LSP completions –
;; notably, JSON Schema completions.


(package! yasnippet
  (keymap-set ceamx-insert-prefix-map "s" #'yas-insert-snippet)

  (defer! 3
    (yas-global-mode 1)))

(after! yasnippet
  (setopt yas-snippet-dirs
          (list (file-name-concat ceamx-templates-dir "yasnippet")))
  (setopt yas-prompt-functions '(yas-completing-prompt
                                 yas-no-prompt)))

;; Disable automatic whitespace modifications in snippet files

;; <https://joaotavora.github.io/yasnippet/faq.html#org64f1b8c>

;; #+begin_quote
;; If there is a newline at the end of a snippet definition file,
;; YASnippet will add a newline when expanding that snippet. When editing
;; or saving a snippet file, please be careful not to accidentally add a
;; terminal newline.
;; #+end_quote


(defun +yasnippet-snippet-mode-disable-final-newline-h ()
  "Prevent appendage of a final newline in `snippet-mode' files.
A final newline would be inserted literally into the snippet expansion."
  (setq-local require-final-newline nil))

(add-hook 'snippet-mode-hook #'+yasnippet-snippet-mode-disable-final-newline-h nil t)

;; Customize the template for newly-created snippets with ~yas-new-snippet-default~


(after! yasnippet
  (setopt
   yas-new-snippet-default
   "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}\n# uuid: `(uuidgen-4)`\n# contributor: astratagem <chmont@protonmail.com>\n# --\n$0`(yas-escape-text yas-selected-text)`"))

;; =spdx= :: insertable SPDX license headers
;; :PROPERTIES:
;; :ID:       4f029a65-d064-4715-9947-e9d32b4bdf67
;; :END:

;; - src :: <https://github.com/condy0919/spdx.el>


(package! spdx
  (keymap-set ceamx-insert-prefix-map "L" #'spdx-insert-spdx))

;; =corfu= :: [co]mpletion in [r]egion [fu]nction
;; :PROPERTIES:
;; :ID:       d8073181-6d05-40d2-a954-0e6bb65449a2
;; :END:

;; + Package :: <https://github.com/minad/corfu>
;; + Reference :: <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>


(package! corfu
  (defer! 5 (require 'corfu))

  (add-hook 'ceamx-after-init-hook #'global-corfu-mode))

(after! corfu
  (setopt corfu-count 12
          corfu-cycle t
          ;; corfu-max-width 80
          corfu-min-width 20
          corfu-scroll-margin 0)
  ;; cf. `orderless-component-separator'
  (setopt corfu-separator ?_)
  (setopt corfu-on-exact-match 'insert
          corfu-preselect 'prompt
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match t)
  (setopt corfu-preview-current t)
  (setopt corfu-echo-delay '(0.3 . 0.3))
  (setopt corfu-popupinfo-delay '(0.75 . 0.5))
  (setopt corfu-auto t
          corfu-auto-delay 1.0
          ;; corfu-auto-delay 1.3
          corfu-auto-prefix 4)

  ;; Disable Corfu in the minibuffer when another completion UI is
  ;; active or when entering secrets.
  (setopt global-corfu-minibuffer
      (lambda ()
        (not (or (bound-and-true-p mct--active)
                 (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map)))))

  ;; Setting this here again for good measure, just in case it is
  ;; changed elsewhere.
  (setopt tab-always-indent 'complete)

  ;; Prevent excessive completion-spamming.
  ;; Without this, on Emacs 30.0, typing causes constant `corfu' errors.
  ;; <https://github.com/minad/corfu/discussions/457>
  (setopt text-mode-ispell-word-completion nil)

  (define-keymap :keymap corfu-map
     "M-SPC" #'corfu-insert-separator
     "M-a" #'corfu-reset
     "RET" #'corfu-complete
     "<tab>" #'corfu-next
     "<backtab>" #'corfu-previous
     "<escape>" #'corfu-reset)


  ;; (when (eq 'complete tab-always-indent)
  ;;   (keymap-set corfu-map "TAB" #'corfu-complete))

  (corfu-popupinfo-mode 1)
  (corfu-echo-mode -1)

  ;; Sort candidates by input history.
  (after! savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; =corfu-terminal= :: Corfu terminal support

;; + Package :: <https://codeberg.org/akib/emacs-corfu-terminal>

;; Corfu-endorsed solution to making it usable in terminal.

;; See also ~popon~, the utility library powering the interface.


(package! corfu-terminal
  (after! (corfu)
    (unless (display-graphic-p)
      (corfu-terminal-mode 1))))

;; =mct= :: Minibuffer Confines Transcended


(package! mct
  (setopt mct-live-completion t
          mct-live-update-delay 0.6)
  (setopt mct-minimum-input 3)
  (setopt mct-completion-window-size (cons #'mct-frame-height-third 1))
  (setopt mct-completing-read-multiple-indicator t)
  (setopt mct-hide-completion-mode-line t)

  ;; `file-name-shadow-mode' must be enabled for this option to have an
  ;; effect.  It should be enabled by default.
  (setopt mct-remove-shadowed-file-names t)

  ;; This is for commands or completion categories that should always
  ;; pop up the completions' buffer.  It circumvents the default method
  ;; of waiting for some user input (see `mct-minimum-input') before
  ;; displaying and updating the completions' buffer.
  (setopt mct-completion-passlist
          '(;; Some commands
            select-frame-by-name
            Info-goto-node
            Info-index
            Info-menu
            vc-retrieve-tag
            ;; Some completion categories
            consult-buffer
            consult-location
            embark-keybinding
            imenu
            file
            project-file
            buffer
            kill-ring))

  ;; The blocklist follows the same principle as the passlist, except it
  ;; disables live completions altogether.
  (setopt mct-completion-blocklist nil)

  ;; This is the default value but I am keeping it here for visibility.
  (setopt mct-sort-by-command-or-category
        '((file . mct-sort-by-directory-then-by-file)
          ((magit-checkout vc-retrieve-tag) . mct-sort-by-alpha-then-by-length)
          ((kill-ring imenu consult-location Info-goto-node Info-index Info-menu) . nil) ; no sorting
          (t . mct-sort-by-history))))

;; =kind-icon= :: icons for ~completion-at-point~ candidates :icons:
;; :PROPERTIES:
;; :ID:       7153b3e8-34f4-47c1-a1e7-6de207be7d29
;; :END:

;; + Package :: <https://github.com/jdtsmith/kind-icon>


(package! kind-icon
  (require 'kind-icon)

  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-blend-background t)

  (after! corfu
    (setopt kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(after! kind-icon
  ;; Reduce the icon size slightly.  If you change this value, make
  ;; sure to call `kind-icon-reset-cache' afterwards, otherwise the
  ;; icon size will likely not be accurate:
  (plist-put kind-icon-default-style :height 0.9)

  ;; Update icon appearance after enabling a theme.
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'enable-theme-functions (lambda (_) (kind-icon-reset-cache))))

;; =cape= :: [c]ompletion-[a]t-[p]oint [e]xtensions :capfs:
;; :PROPERTIES:
;; :ID:       e7028330-f02c-4862-ac3a-054f70fb9e92
;; :END:


(package! cape
  ;; Add to the global default value of
  ;; `completion-at-point-functions' which is used by
  ;; `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  We use `add-hook' due to
  ;; its ability to specify priority values affecting order.  Note
  ;; that the list of buffer-local completion functions takes
  ;; precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)

  (add-to-list 'completion-category-overrides
               '((cape-dict (styles (basic)))))

  (after! eglot
    ;; NOTE: This may cause a significant performance hit.  Consider
    ;; enabling per-language-server.
    ;; <https://github.com/minad/corfu/wiki#continuously-update-the-candidates>
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (keymap-set ceamx-insert-prefix-map "E" #'cape-emoji)

  ;; cf. `cape-prefix-map' for ideas
  (define-keymap :keymap ceamx-completion-prefix-map
    "p" #'completion-at-point

    "a" #'cape-abbrev
    "d" #'cape-dabbrev
    "e" (cape-capf-interactive #'elisp-completion-at-point)
    "f" #'cape-file
    "o" #'cape-elisp-symbol
    "w" #'cape-dict))

;; =embark= :: [e]macs [m]ini-[b]uffer [a]ctions [r]ooted in [k]eymaps :embark:
;; :PROPERTIES:
;; :ID:       111a0c90-a300-4fa8-a954-6d5e97fcea89
;; :END:

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

  ;; This setting only affects the behavior of `embark-act' inside the
  ;; minibuffer.  You can reverse the configured behavior at any time
  ;; by calling `embark-act' with a "C-u" prefix argument.
  ;;
  ;; For finer control, e.g.: `((kill-buffer . t) (t . nil))'
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

;; Keybinding help with Embark’s ~embark-prefix-help-command~
;; :PROPERTIES:
;; :ID:       4ddaa528-5c3b-494a-9aa0-95e32a93fb8f
;; :END:


(after! embark
  (setopt prefix-help-command #'embark-prefix-help-command)
  (after! vertico
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))))

;; Add command to export completion candidates to a writable buffer :embark:


(use-feature! ceamx-completion
  :after minibuffer
  :commands (ceamx-completion/embark-export-write)
  :init
  (keymap-set minibuffer-local-map "C-c C-e" #'ceamx-completion/embark-export-write))

;; Keybindings
;; :PROPERTIES:
;; :ID:       e5538115-fecd-4e47-bd55-d9e0fb112313
;; :END:


(keymap-global-set "C-c p" #'completion-at-point)

;; Provide feature ~ceamx-init-completion~
;; :PROPERTIES:
;; :ID:       f58a3ca1-4cbd-4220-9d65-ad102ed548c9
;; :END:


(provide 'ceamx-init-completion)
;;; ceamx-init-completion.el ends here
