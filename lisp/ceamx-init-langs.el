;; -*- lexical-binding: t -*-

(require 'ceamx-lib)

;; Programming modes


(after! prog-mode
  (define-keymap :keymap prog-mode-map
    ;; Move forward out of one sexp level
    "C-M-d" #'up-list))

(def-hook! ceamx-init-lang-prog-mode-h ()
  '(prog-mode-hook)
  "Enable load-order-dependent features and defaults for all `prog-mode'-derived major modes.
To ensure a predictable execution order, condition checks on
`boundp'/`fboundp' are preferable to using `after!' or
`with-eval-after-load', as the latter may result in a non-deterministic
execution order."

  ;; `highlight-function-calls-mode' should be enabled after other highlighters
  ;; (e.g. `rainbow-delimiters-mode'), according to its readme.
  (when (fboundp 'highlight-function-calls-mode)
    (highlight-function-calls-mode 1)))

;; ~smart-newline~: a newline command for programming modes


(use-package smart-newline
  ;; :ensure t
  :hook (prog-mode . smart-newline-mode))

;; ~dumb-jump~: multi-lang do-what-i-mean jump-to-definition

;; - Package :: <https://github.com/jacktasia/dumb-jump>


(package! dumb-jump
  ;; Add to end of `xref-backend-functions' as a dumb fallback when
  ;; there are no smart options.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100)

  (after! (hydra)
    ;; via <https://github.com/jacktasia/dumb-jump?tab=readme-ov-file#hydra-for-effieciency>
    (defhydra ceamx-prog-dumb-jump-dispatch (:color blue :columns 3)
      "Jump (dumbly)"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back"))))

;; ~rainbow-mode~: Colorize color names and hexcodes in buffers :theme:

;; <https://elpa.gnu.org/packages/rainbow-mode.html>


(package! rainbow-mode)

;; ~hl-todo~: Highlight "TODO" and other codetags in comments and strings

;; - website :: <https://github.com/tarsius/hl-todo>
;; - reference :: <https://peps.python.org/pep-0350/#specification>


(use-package hl-todo
  ;;:ensure t
  :hook (prog-mode . hl-todo-mode))

;; ~devdocs~: Peruse local ~devdocs~ docsets

;; - Source code :: <https://github.com/astoff/devdocs.el>

;; NOTE: Must run ~devdocs-install~ before a docset is available for reference.


(use-package devdocs
  ;; :ensure t
  :defer t

  :bind
  (:map help-map
        ("D" . devdocs-lookup))

  :init
  (after! popper
    (add-to-list 'popper-reference-buffers "\\*devdocs\\*"))

  :config
  ;; FIXME: on a stale timer! every week! not every session...
  ;; (devdocs-update-all)

  )



;; WIP: Define some helper functions for installing docsets automatically:


(defun +devdocs--doc-directory-exists-p (slug)
  "Whether the directory for the doc SLUG exists."
  (file-directory-p (expand-file-name slug devdocs-data-dir)))

(defun +devdocs--doc-installed-p (slug)
  "Whether the document named SLUG is installed.
Installation can be defined as whether there exists a metadata
file inside a directory named SLUG within `devdocs-data-dir'."
  (defvar devdocs-data-dir)
  (let ((file (expand-file-name (concat slug "/metadata") devdocs-data-dir)))
    (file-exists-p file)))

(defun +devdocs-maybe-install (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is as in `devdocs-install'."
  (declare-function devdocs-install "devdocs")
  (unless (+devdocs--doc-installed-p doc)
    (devdocs-install doc)))

(defun +devdocs-maybe-install-docs (docs)
  "Install each `devdocs' documentation set in DOCS if not already installed.
DOCS is a quoted list of `devdocs' documentation identifiers as
accepted by `+devdocs-maybe-install'."
  (dolist (doc docs)
    (+devdocs-maybe-install doc)))

;; FIXME: return t if exists, whatever if new, otherwise throw
(defun ceamx/devdocs-maybe-install (doc)
  "Install the `devdocs' documentation set for DOC if not already installed.
DOC is as in `devdocs-install'."
  ;; TODO: prompt for selecting from available docs (see `devdocs-install')
  (interactive "s")
  (+devdocs-maybe-install doc))

;; Display multiple composed messages inside ~eldoc~


(setopt eldoc-documentation-function #'eldoc-documentation-compose)

;; Linting files with the builtin ~flymake~ feature


(setup flymake
  (:hook-into ceamx-after-init-hook)
  ;; Mirror the [C-c !] Flycheck prefix.
  (:bind "C-c ! l" #'flymake-show-buffer-diagnostics
         "C-c ! n" #'flymake-goto-next-error
         "C-c ! p" #'flymake-goto-previous-error
         "C-c ! c" #'flymake-show-buffer-diagnostics)
  (:when-loaded
    (setopt flymake-fringe-indicator-position 'right-fringe)
    (setopt flymake-no-changes-timeout 1.0)
    (setopt flymake-wrap-around t)))

;; ~Linting files with the ~flycheck~ package :lint:


(package! flycheck
  (add-hook 'ceamx-after-init-hook #'global-flycheck-mode))

(package! consult-flycheck
  (keymap-global-set "M-g f" #'consult-flycheck)

  (after! (consult flycheck)
    (require 'consult-flycheck)))

(after! flycheck
  (setopt flycheck-emacs-lisp-load-path 'inherit)
  (setopt flycheck-idle-change-delay 3.0
          flycheck-display-errors-delay 1.5)
  (setopt flycheck-check-syntax-automatically
          '(save idle-change mode-enabled))
  (setopt flycheck-buffer-switch-check-intermediate-buffers nil)

  ;; Disable Flycheck for modes supported by Flymake
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(emacs-lisp
                          emacs-lisp-checkdoc
                          emacs-lisp-package
                          sh-shellcheck))))



;; #+RESULTS:
;; | en                   | (aspell) |
;; | en-variant_0         | (aspell) |
;; | en-variant_1         | (aspell) |
;; | en-variant_2         | (aspell) |
;; | en-w_accents         | (aspell) |
;; | en-wo_accents        | (aspell) |
;; | en_AU                | (aspell) |
;; | en_AU-variant_0      | (aspell) |
;; | en_AU-variant_1      | (aspell) |
;; | en_AU-w_accents      | (aspell) |
;; | en_AU-wo_accents     | (aspell) |
;; | en_CA                | (aspell) |
;; | en_CA-variant_0      | (aspell) |
;; | en_CA-variant_1      | (aspell) |
;; | en_CA-w_accents      | (aspell) |
;; | en_CA-wo_accents     | (aspell) |
;; | en_GB                | (aspell) |
;; | en_GB-ise            | (aspell) |
;; | en_GB-ise-w_accents  | (aspell) |
;; | en_GB-ise-wo_accents | (aspell) |
;; | en_GB-ize            | (aspell) |
;; | en_GB-ize-w_accents  | (aspell) |
;; | en_GB-ize-wo_accents | (aspell) |
;; | en_GB-variant_0      | (aspell) |
;; | en_GB-variant_1      | (aspell) |
;; | en_GB-w_accents      | (aspell) |
;; | en_GB-wo_accents     | (aspell) |
;; | en_US                | (aspell) |
;; | en_US-variant_0      | (aspell) |
;; | en_US-variant_1      | (aspell) |
;; | en_US-w_accents      | (aspell) |
;; | en_US-wo_accents     | (aspell) |
;; | he                   | (hspell) |
;; | he_IL                | (hspell) |


(autoload 'global-jinx-mode "jinx")
(autoload 'jinx-correct "jinx")
(autoload 'jinx-languages "jinx")

(add-hook 'ceamx-emacs-startup-hook #'global-jinx-mode)

(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-M-$" #'jinx-languages)

(after! jinx
  (setopt jinx-languages "en"))

;; [[https://github.com/purcell/emacs-reformatter][purcell/emacs-reformatter]]: KISS DIY FMT :package:


(package! reformatter
  (require 'reformatter))

;; =biome=

;; - Docs :: <https://biomejs.dev/guides/integrate-in-editor/>

;; This should, possibly more ideally, be run as an LSP client, but AFAIK one does
;; not yet exist for Eglot (only LSP-Mode).


(after! reformatter
  (reformatter-define biome-format
    :program "biome"
    :args (list "format" "--stdin-file-path" (buffer-file-name))))

;; =prettier=

;; - Source :: <https://github.com/akirak/flake-templates/blob/629b04932dc71e3e0213d66a0aa8a08cd0b64922/README.md#emacs>


(after! reformatter
  (reformatter-define prettier
    :program "prettier"
    :args (list (concat "--plugin-search-dir="
                        (expand-file-name
                         (locate-dominating-file default-directory "package.json")))
                "--stdin-filepath" (buffer-file-name))))

;; treefmt


(after! reformatter
  (reformatter-define treefmt
    :group 'ceamx
    :program "treefmt"
    :args (list "--stdin" (buffer-file-name))))

(after! popper
  (push "\\*treefmt-errors\\*" popper-reference-buffers))

;; [[https://github.com/radian-software/apheleia][radian-software/apheleia]]: opinionated code reformatting :package:

;; In case you run into issues with ~web-mode~ not updating syntax highlighting after
;; formatting (or other arbitrary modifications):
;; <https://github.com/doomemacs/doomemacs/blob/35dc13632b3177b9efedad212f2180f69e756853/modules/editor/format/config.el#L74-L83>


(package! apheleia
  ;; (apheleia-global-mode 1)
  )

(after! (apheleia)
  (blackout 'apheleia-mode " Aph"))

;; Use the Biome formatter for supported major modes :lang:

;; - Reference :: <https://biomejs.dev/internals/language-support/>

;; As of <2024-05-24 Fri>


;; As of <2024-05-24 Fri>
;; <https://biomejs.dev/internals/language-support/>
(defconst ceamx-editor-format-biome-modes-list
  '(javascript-mode js-mode js-ts-mode js3-mode
    typescript-mode typescript-ts-mode
    js-jsx-mode tsx-ts-mode
    json-mode json-ts-mode)
  "List of major-mode symbols for the languages supported by the Biome formatter.")

(after! reformatter
  (require 'derived)
  (dolist (hook (mapcar #'derived-mode-hook-name ceamx-editor-format-biome-modes-list))
    (add-hook hook #'biome-format-on-save-mode)))

(after! apheleia
  (add-to-list 'apheleia-formatters '(biome "biome" "format" "--stdin-file-path" filepath))

  (dolist (mode ceamx-editor-format-biome-modes-list)
    (add-to-list 'apheleia-mode-alist '(mode . biome))))

;; Define user option to disable format-on-save for some modes


(defcustom ceamx-format-on-save-disabled-modes
  '(emacs-lisp-mode                     ; conflict with `lispy' indent
    org-msg-edit-mode)
  "A list of major modes in which to not reformat the buffer upon saving.
When nil, buffers will always be formatted upon save. When
non-nil, buffers will never be formatted upon save."
  :group 'ceamx
  :type '(choice boolean (repeat symbol)))

;; Inhibit automatic formatting in some contexts

;; Unlike ~reformatter~, ~apheleia~ will /always/ run if it can.  A blessing and a curse.
;; This section handles the curse.


(defun ceamx-editor-format-maybe-inhibit-h ()
  "Check if formatting should be disabled for current buffer."
  (or (eq major-mode 'fundamental-mode)
      (string-blank-p (buffer-name))
      (eq ceamx-format-on-save-disabled-modes t)
      (not (null (memq major-mode ceamx-format-on-save-disabled-modes)))))

(after! (apheleia)
  (add-to-list 'apheleia-inhibit-functions #'ceamx-editor-format-maybe-inhibit-h))

;; Inhibit on-save formatting with prefix argument

;; - Source :: <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L2266-L2270>


(after! apheleia
  (def-advice! +apheleia-save-buffer-maybe-reformat-a (func &optional arg)
    :around #'save-buffer
    "Inhibit reformatting-on-save when providing a prefix argument to \\[save-buffer]."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func))))

;; Define the user option for structured editing flavour


(defcustom ceamx-structured-editing-style 'lispy
  "The structured editing provider."
  :group 'ceamx
  :type '(choice :tag "Structured editing style" :value lispy
          (const :tag "Lispy" lispy)
          (const :tag "Puni" puni)))

;; ~puni~: versatile structured editing :package:

;; <https://github.com/AmaiKinono/puni>


(package! puni
  ;; (puni-global-mode)
  ;; (add-hook 'prog-mode-hook #'puni-mode)
  ;; (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  )

;; (after! puni
;;     ;; (define-keymap :keymap puni-mode-map
;;   ;;   "C-M-f" #'puni-forward-sexp
;;   ;;   "C-M-b" #'puni-backward-sexp
;;   ;;   "C-M-a" #'puni-beginning-of-sexp
;;   ;;   "C-M-e" #'puni-end-of-sexp
;;   ;;   "C-M-[" #'puni-backward-sexp-or-up-list
;;   ;;   "C-M-]" #'puni-forward-sexp-or-up-list

;;   ;;   "M-(" #'puni-syntactic-forward-punct
;;   ;;   "M-)" #'puni-syntactic-backward-punct
;;   ;;   )

;; )

;; Prepare a prefix commands for binding structural editing commands


(define-prefix-command 'ceamx-structural-editing-prefix)
(keymap-global-set "C-c s" #'ceamx-structural-editing-prefix)

;; Structural editing with ~puni~

;; Work in progress.

;; This is still not quite usable as a Lispy replacement.  The goal is to use
;; similar structureal editing keybindings across many major-modes.

;; Note that this repeat-map should not be used in tandem with ~lispy-mode~ because
;; its bindings generally would need ~puni-mode~ to be active.

;; - <https://karthinks.com/software/a-consistent-structural-editing-interface/>
;; - <https://github.com/suliveevil/emacs.d?tab=readme-ov-file#repeat-repeat-mode>
;; - <https://github.com/karthink/.emacs.d/blob/master/init.el#L3209-L3241>


;; - [ ] Disable ~repeat-exit-timeout~ for this map only


(after! puni

  (defvar-keymap structural-editing-map
    :repeat t

    "d" #'puni-forward-delete-char
    ;; "DEL" #'puni-backward-delete-char
    ;; "D" #'puni-forward-kill-word
    ;; "M-DEL" #'puni-backward-kill-word
    ;; "C-k" #'puni-kill-line
    ;; "M-k" #'puni-backward-kill-line
    "k" #'kill-sexp

    "f" #'puni-forward-sexp
    "b" #'puni-backward-sexp
    "[" #'puni-backward-sexp-or-up-list
    "]" #'puni-forward-sexp-or-up-list
    "a" #'puni-beginning-of-sexp
    "e" #'puni-end-of-sexp
    "u" #'puni-up-list
    "M-(" #'puni-syntactic-forward-punct
    "M-)" #'puni-syntactic-backward-punct

    "\\" #'indent-region
    "/" #'undo

    ">" #'puni-slurp-forward
    "<" #'puni-slurp-backward
    "}" #'puni-barf-forward
    "{" #'puni-barf-backward
    "R" #'puni-raise
    "t" #'puni-transpose
    "C" #'puni-convolute
    ;; FIXME: avoid meow dependency -- no puni equivalent
    ;; "J" #'meow-join-sexp
    "S" #'puni-split
    ;; FIXME: for `emacs-lisp-mode' only
    "x" #'eval-defun

    ))

;; FIXME: wrong type argument symbolp
;; (map-keymap (lambda (_ cmd)
;;               (put cmd 'repeat-exit-timeout nil)) structural-editing-map)

;; General customizations for ~outline-mode~ and ~outline-minor-mode~

;; Remember that ~org-mode~ is derived from ~outline-mode~.


(keymap-global-set "<f9>" #'outline-minor-mode)

(after! outline
  (setopt outline-minor-mode-highlight t)
  (setopt outline-minor-mode-cycle t)
  (setopt outline-minor-mode-use-buttons nil))

;; ~outli~ :: mode-agnostic comment-based outlines

;; - Package :: <https://github.com/jdtsmith/outli>


(package! (outli :host github :repo "jdtsmith/outli")
  (def-hook! +outli-mode-maybe-enable-h ()
    '(prog-mode-hook text-mode-hook)
    "Enable `outli-mode' conditionally, excluding some modes.
Note that `emacs-lisp-mode' is excluded here due to a conflict with
`lispy-mode'.  `outli-mode' must be loaded after `lispy-mode'."
    (let ((exclude-modes '(emacs-lisp-mode))
          (excludep (lambda (excluded-mode)
                      (eq major-mode excluded-mode))))
      (unless (seq-some excludep exclude-modes)
        (outli-mode)))))

(after! outli
  ;; FIXME: this example from the readme results in errors due to mismatched signature
  ;; (advice-add 'load-theme :after #'outli-reset-all-faces)
  ;; (advice-remove 'load-theme #'outli-reset-all-faces)

  (define-keymap :keymap outli-mode-map
    "C-c C-n" #'outline-next-heading
    "C-c C-p" #'outline-previous-heading
    ;; "C-c C-p" #'outline-back-to-heading
    "C-c M-h" #'outline-promote
    "C-c M-l" #'outline-demote))

;; TODO Expand the existing repeat map for outline navigation

;; I think this needs to be /contracted/ a bit.  Some of these aren't even real commands.


(after! (repeat outline)
  (define-keymap :keymap outline-navigation-repeat-map
    "C-x" #'foldout-exit-fold
    "x" #'foldout-exit-fold
    "C-z" #'foldout-zoom-subtree
    "z" #'foldout-zoom-subtree
    "C-a" #'outline-show-all
    "a" #'outline-show-all
    "C-c" #'outline-hide-entry
    "c" #'outline-hide-entry
    "C-d" #'outline-hide-subtree
    "C-e" #'outline-show-entry
    "e" #'outline-show-entry
    "TAB" #'outline-show-children
    "C-k" #'outline-show-branches
    "k" #'outline-show-branches
    "C-l" #'outline-hide-leaves
    "l" #'outline-hide-leaves
    "RET" #'outline-insert-heading
    "C-o" #'outline-hide-other
    "o" #'outline-hide-other
    "C-q" #'outline-hide-sublevels
    "q" #'outline-hide-sublevels
    "C-s" #'outline-show-subtree
    "s" #'outline-show-subtree
    "C-t" #'outline-hide-body
    "t" #'outline-hide-body
    "@" #'outline-mark-subtree)

  (ceamx-repeatify-keymap 'outline-navigation-repeat-map))

;; TODO A transient menu for outline navigation


;; (after! (transient outline)
;;   (transient-define-prefix ceamx/outline-dispatch ()
;;     "Outline navigation transient menu."
;;     [["Navigate"
;;       ("u" "up" outline-up-heading)
;;       ("n" "next" outline-next-visible-heading)
;;       ("p" "prev" outline-previous-visible-heading)
;;       ("f" "forward" outline-forward-same-level)
;;       ("b" "backward" outline-backward-same-level)]]))

;; (after! (hydra outline)
;;   (defhydra ceamx/outline-hydra ( :color red)
;;     "
;; ^Navigate^            ^Subtree^        ^Metadata^
;; ^--------^----------  ^-------^-----  ^---------^--
;; _n_ext visible        _I_: drag up    _t_odo-state
;; _p_revious visible    _J_: promote    _d_eadline
;; _f_orward same level  _K_: drag down  _s_chedule
;; _b_ack same level     _L_: demote
;; _u_p level            _N_: narrow     _xp_: set property
;;                       _W_: widen
;; "))

;; Apply ~autoinsert~ skeletons to new files


(use-feature! autoinsert
  :config
  (auto-insert-mode 1))

;; Code


(require 'derived)

(require 'lib-lisp)

;; Configure behavior for all Lisp modes with ~ceamx-lisp-init-hook~


(add-hook 'ceamx-lisp-init-hook #'ceamx-enable-check-parens-on-save)

;; Add hooks to supported Lisp modes.
(dolist (mode ceamx-lisp-modes-list)
  (add-hook (derived-mode-hook-name mode) #'ceamx-lisp-init))

;; Always indent Lisp code with two spaces

;; Even if GNU Emacs defaults sometimes do things differently.


(dolist (sym '(add-function add-to-list advice-add plist-put))
  (put sym 'lisp-indent-function 2))

;; ~lispy~: the structural expression editing experience

;; - Website :: [[https://github.com/abo-abo/lispy][GitHub - abo-abo/lispy: Short and sweet LISP editing]]
;; - API Reference :: [[https://oremacs.com/lispy/][lispy.el function reference]]


(package! lispy
  (when (eq 'lispy ceamx-structured-editing-style)
    (add-hook 'ceamx-lisp-init-hook #'lispy-mode)))

(after! lispy
    ;; Prevent `lispy' from inserting escaped quotes when already inside a string,
    ;; in favor of just moving past the closing quote as I would expect.
    ;;
    ;; FIXME: This actually results in creating the quote pair *after* the
    ;; closing quote. "for example:"" "
;;    (setopt lispy-close-quotes-at-end-p t)

    (setopt lispy-completion-method 'default)

    (setopt lispy-eval-display-style 'message)

    ;; I have mixed feelings about this one because it can be jarring and easily
    ;; lead to mass-commenting expressions. Default is non-nil.
    (setopt lispy-move-after-commenting t)

    (define-keymap :keymap lispy-mode-map
      "M-j" nil                         ; shadows custom binding

      ;; via <https://github.com/abo-abo/lispy/pull/619>
      "`" #'self-insert-command)

    (after! outli
      ;; `outli-mode' overrides `lispy-mode' outline functionality, so it must
      ;; be activated afterwards.
      (add-hook 'ceamx-lisp-init-hook #'outli-mode))

    (after! macrostep
      (push 'macrostep lispy-compat))

    (after! popper
      (push "\\*lispy-message\\*" popper-reference-buffers)))

;; ~kbd-mode~: syntax support for =kmonad= and =kanata= configs

;; [[https://github.com/kmonad/kbd-mode][GitHub - kmonad/kbd-mode: Emacs mode for syntax highlighting kmonad's .kbd files.]]


(package! (kbd-mode :host github :repo "kmonad/kbd-mode"))

;; Inhibit formatters

;; Unfortunately, we need to do this because whitespace is used to convey
;; non-syntactic meaning to the reader.


(after! kbd-mode
  (add-to-list 'ceamx-format-on-save-disabled-modes #'kbd-mode)
  (after! lispy
    (add-to-list 'lispy-no-indent-modes #'kbd-mode)))

;; General Elisp support customizations


(defun ceamx-emacs-lisp-init ()
  "Sensible defaults for `emacs-lisp-mode'."
  (ceamx-lisp-init)
  (eldoc-mode 1)
  ;; TODO: do we really want this for `ielm' and other derived modes as well?
  (blackout "EL"))

(add-hook 'emacs-lisp-mode-hook #'ceamx-emacs-lisp-init)
(add-hook 'ielm-mode-hook #'ceamx-emacs-lisp-init)

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;; Use current ~load-path~ during compilation to tone down Flymake

;; + Source :: <https://github.com/doomemacs/doomemacs/blob/98d753e1036f76551ccaa61f5c810782cda3b48a/modules/lang/emacs-lisp/config.el#L124C1-L138C15>


(def-advice! +elisp-flymake-byte-compile-fix-load-path-a (orig-fn &rest args)
  :around #'elisp-flymake-byte-compile
  "Set load path for the `emacs-lisp' byte compilation `flymake' backend."
  (let ((elisp-flymake-byte-compile-load-path
         (append elisp-flymake-byte-compile-load-path load-path)))
    (apply orig-fn args)))

;; Display variable value next to ~eldoc~ output

;; + Source :: <https://github.com/doomemacs/doomemacs/blob/98d753e1036f76551ccaa61f5c810782cda3b48a/modules/lang/emacs-lisp/config.el#L124C1-L138C15>


(def-advice! +emacs-lisp-append-value-to-eldoc-a (fn sym)
  :around #'elisp-get-var-docstring
  "Display variable value next to documentation in eldoc."
  (when-let (ret (funcall fn sym))
    (if (boundp sym)
        (concat ret " "
                (let* ((truncated " [...]")
                       (print-escape-newlines t)
                       (str (symbol-value sym))
                       (str (prin1-to-string str))
                       (limit (- (frame-width) (length ret) (length truncated) 1)))
                  (format (format "%%0.%ds%%s" (max limit 0))
                          (propertize str 'face 'warning)
                          (if (< (length str) limit) "" truncated))))
      ret)))

;;; Keybinds

(keymap-global-set "<remap> <indent-pp-sexp>" #'ceamx/indent-last-sexp)

(define-keymap :keymap emacs-lisp-mode-map
  "C-:" #'ielm

  "C-S-t" #'transpose-sexps)

(with-eval-after-load 'ielm
  (defvar ielm-map)
  (keymap-set ielm-map "C-:" #'quit-window))

;; ~eros~ :: [E]valuation [R]esult [O]verlay[S]

;; + Website :: <https://github.com/xiongtx/eros>


(use-package eros
  :commands (eros-mode eros-eval-last-sexp)
  :init
  (add-hook 'emacs-lisp-mode-hook #'eros-mode)
  (keymap-set emacs-lisp-mode-map "<remap> <eval-last-sexp>" #'eros-eval-last-sexp)

  (use-feature! lispy
    :autoload (lispy-define-key)
    :config
    (def-hook! +lispy-use-eros-eval-h () 'lispy-mode-hook
      "Use `eros-eval-last-sexp' in place of `lispy-eval' bindings."
      ;; FIXME: there is currently no way to hide lispy-eval output.
      ;;        nil results in an error.
      ;;        because of this, output is duplicated in the minibuffer and the
      ;;        eros overlay...
      ;;
      ;; (setopt lispy-eval-display-style nil)
      (lispy-define-key lispy-mode-map "e" #'eros-eval-last-sexp))))

;; ~suggest~ :: meet the elisp function of your dreams

;; + Package :: <https://github.com/Wilfred/suggest.el>


(use-package suggest
  :commands (suggest)
  :init
  (keymap-set emacs-lisp-mode-map "C-c S" #'suggest))

;; ~macrostep~ :: interactive macro-explorer

;; + Package :: <https://github.com/emacsorphanage/macrostep>


(use-package macrostep
  :commands (macrostep-expand)

  :preface
  ;; <https://github.com/joddie/macrostep/issues/11>
  ;; <https://github.com/emacsorphanage/macrostep/issues/8>
  (defun ceamx/macrostep-expand ()
    "Wrapper for `macrostep-expand' providing workaround for errors.
The original function fails in the presence of whitespace after a sexp."
    (interactive)
    (when (and (= ?\n (char-after))
               (= (point) (cdr (bounds-of-thing-at-point 'sexp))))
      (backward-char))
    (macrostep-expand))

  :init
  (keymap-set emacs-lisp-mode-map "C-c x" #'ceamx/macrostep-expand))

;; ~xr~ :: convert string regexps to ~rx~ forms

;; + Package :: <https://github.com/mattiase/xr>


(package! xr)

;; ~elmacro~: Display keyboard macros or latest interactive commands as Elisp

;; + Package :: <https://github.com/Silex/elmacro>

;; Avoid enabling this mode globally.  It may cause some recurring
;; errors, and the package has not been updated in years.  By nature, it
;; is also quite invasive, and should probably only be used as a
;; development tool as needed.


(use-package elmacro
  ;; :ensure t
  :config
  (setopt elmacro-show-last-commands-default 30)

  ;; <https://github.com/Silex/elmacro/blob/master/README.md#org-mode-smartparens-etc>
  ;; <https://github.com/Silex/elmacro/blob/master/README.md#elmacro-processor-prettify-inserts>
  (setopt elmacro-processor-prettify-inserts
          (unless (or (bound-and-true-p lispy-mode) ; not actually sure about lispy-mode
                      (bound-and-true-p smartparens-mode)
                      (bound-and-true-p org-mode))))

  ;; "a" "b" "c" => "abc"
  ;; FIXME: maybe causes errors?
  (setopt elmacro-processor-concatenate-inserts t))

;; ~elisp-demos~ :: display elisp usage examples inside help buffers

;; - Package :: <https://github.com/xuchunyang/elisp-demos>


(use-package elisp-demos
  ;; :ensure t
  :defer 5
  :after (helpful)
  :functions (elisp-demos-advice-helpful-update)
  :config
  (setopt elisp-demos-user-files (list (expand-file-name  "docs/elisp-demos.org" user-emacs-directory)))

  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; ~morlock~ :: more font-lock keywords for some eldritch expressions

;; + Package :: <https://github.com/tarsius/morlock>


(package! morlock
  (global-morlock-mode))

;; ~paren-face~ :: parentheses are beautiful


(package! paren-face
  (global-paren-face-mode))

;; ~keymap-utils~ :: dev library for working with keymaps


(package! keymap-utils)

;; Language Server and Debugger Protocol Support :lsp:lang:


(defcustom ceamx-lsp-client 'eglot
  "The preferred LSP client."
  :group 'ceamx
  :type '(choice :tag "LSP client" :value eglot
          (const :tag "Eglot [builtin]" eglot)
          (const :tag "LSP-Mode" lsp-mode)))

(defvar ceamx-lsp-mode-cache-dir (file-name-as-directory (concat ceamx-var-dir "lsp")))

;; Eglot


(defvar ceamx-eglot-storage-dir (file-name-as-directory (concat ceamx-var-dir "eglot")))

(setopt eglot-sync-connect 1)
(setopt eglot-autoshutdown t)
(setopt eglot-send-changes-idle-time 0.5)

;; Disable events buffer, which poses performance issues over time as the
;; buffer grows in a longer-running Emacs instance.
(setopt eglot-events-buffer-size 0)

;; Prevent frequent focus-stealing.
(setopt eglot-auto-display-help-buffer nil)

;; Use =emacs-lsp-booster= via ~eglot-booster~ :perf:

;; - Website :: <https://github.com/jdtsmith/eglot-booster>
;; - Website :: <https://github.com/blahgeek/emacs-lsp-booster>

;; Requires =emacs-lsp-booster= to be installed into the environment.  Available by that name in Nixpkgs.


(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :commands (eglot-booster-mode)
  :init
  (after! eglot
    (eglot-booster-mode)))



;; Though I have not tried it, I am thinking that using =lsp-booster= over TRAMP is
;; not worth the trouble of ensuring that the executable is available on every
;; remote server.  At least not as a default behavior.  Consider enabling this
;; per-project or server as desired.

;; elsp

(setopt eglot-booster-no-remote-boost t)

;; Run language servers automatically in supported major modes

;; The timing here may be delicate...


(add-hook 'prog-mode-hook #'eglot-ensure)

(after! eglot
  (defvar eglot-server-programs)

  (def-advice! +eglot--ensure-available-mode (fn)
    :around #'eglot-ensure
    "Run `eglot-ensure' in supported modes."
    (when (alist-get major-mode eglot-server-programs nil nil
                     (lambda (modes key)
                       (if (listp modes)
                           (member key modes)
                         (eq key modes))))
      (funcall fn))))

;; Define helper functions for specifying server configurations


(defvar ceamx-eglot-server-configurations-alist '()
  "Alist of language server initialization options as accepted in `eglot-server-programs'.")

(defun ceamx-eglot-server-default-settings (name)
  "Return the custom initialization options for the NAME language server."
  (alist-get name ceamx-eglot-server-configurations-alist nil nil #'string=))

(defun ceamx-eglot-server-contact (name &optional program &rest args)
  "Return a contact specification for the language server NAME.
NAME is a string of the \"<lang>-<program>\" format for naming
language servers in Ceamx.  This format is based on the format
commonly used by `lsp-mode'.

PROGRAM and ARGS are as in `eglot-server-programs', which see.

Unless PROGRAM is provided, the program name used in
`eglot-server-programs' will be the value of NAME."
  (let ((options (ceamx-eglot-server-default-settings name))
        (program (or program (string-trim-left name "[[:alpha:]]+-"))))
    ;; The use of `append' here is significant because it will filter out a nil
    ;; value for `options'.
    (append (ensure-list program)
            args
            (when options (list :initializationOptions options)))))

;; Declare some Eglot buffers as popup windows


(after! (eglot popper)
  (defvar popper-reference-buffers)
  (add-to-list 'popper-reference-buffers "^\\*eglot-help"))

;; Configure ~flycheck-eglot~ integration


(package! flycheck-eglot
  (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode))

;; Add workspace symbols as Consult datasource with ~consult-eglot~

;; <https://github.com/mohkale/consult-eglot>


(package! consult-eglot
  (defalias 'ceamx/list-workspace-symbols #'consult-eglot-symbols))

;; Keybindings


(keymap-global-set "C-c l a" '("action.." . eglot-code-actions))
(keymap-global-set "C-c l r" '("rename..." . eglot-rename))
(keymap-global-set "C-c l o" #'consult-eglot-symbols)

(after! eglot
  ;; Override the default binding for `xref-find-apropos'.
  (keymap-set eglot-mode-map "C-M-." #'consult-eglot-symbols))

(after! lsp-mode
    (keymap-global-set "C-c l o" #'consult-lsp-symbols)
    ;; Override the default binding for `xref-find-apropos'.
    (keymap-set lsp-mode-map "C-M-." #'consult-lsp-symbols))

;; Register =taplo= formatter


(after! reformatter
  (reformatter-define toml-taplo-fmt
    :group 'ceamx
    :program "taplo"
    :args (list "format" "--diff"
                "--stdin-filepath" (buffer-file-name)
                "-"))

  (add-hook 'conf-toml-mode-hook #'toml-taplo-fmt-on-save-mode)
  (add-hook 'toml-ts-mode-hook #'toml-taplo-fmt-on-save-mode))

;; LSP-Mode: Use the correct cache base directory


(when (featurep 'lsp-toml)
  (setopt lsp-toml-cache-path (file-name-as-directory
                               (concat ceamx-lsp-mode-cache-dir "server/toml"))))

;; Install the =yaml-mode= package


(package! yaml-mode)

;; TODO Start the LSP server

;; - Note taken on [2024-12-26 Thu 21:21] \\
;;   Needs eglot
;; Install the =yaml-language-server= from Nixpkgs first.


(when (eq 'lsp ceamx-lsp-client)
  (after! (yaml-mode)
    (add-hook 'yaml-mode-hook #'lsp-deferred)
    (add-hook 'yaml-ts-mode-hook #'lsp-deferred)))

;; Add support for YAML Schema validation


(when (eq 'lsp ceamx-lsp-client)
  (setopt lsp-yaml-schemas nil)

  ;; Keep this cached file with all of the other LSP server caches.
  (setopt lsp-yaml-schema-store-local-db
          (file-name-concat ceamx-lsp-mode-cache-dir "server/yaml/lsp-yaml-schemas.json"))

  ;; Download the YAML Schema Store database if not present.
  ;; FIXME: handle periodic updates of cached data
  (after! lsp-yaml
    (defer! 2
      (unless (file-exists-p lsp-yaml-schema-store-local-db)
        (lsp-yaml-download-schema-store-db)))))

;; XML [builtin]


(use-feature! nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'"      ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"        ; xslt, xsd
  :mode "\\.rss\\'"

  :config
  (setq nxml-slash-auto-complete-flag t)
  (setq nxml-auto-insert-xml-declaration-flag t))

;; CSV + TSV


(package! csv-mode)

(after! csv-mode
  (define-keymap :keymap csv-mode-map
    "a" #'csv-align-fields
    "u" #'csv-unalign-fields
    "s" #'csv-sort-fields
    "S" #'csv-sort-numeric-fields
    "k" #'csv-kill-fields
    "t" #'csv-transpose))

;; Web-Mode


(package! web-mode
  ;; TODO: refactor
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  (setopt web-mode-engines-alist
          '(("php" . "\\.phtml\\'")
            ("blade" . "\\.blade\\.")))

  ;; Defer to `electric-pair-mode' or similar.
  (setopt web-mode-enable-auto-pairing nil)

  (setopt web-mode-enable-css-colorization t
          web-mode-enable-block-face t
          web-mode-enable-part-face t
          web-mode-enable-current-element-highlight t))

;;; emmet-mode

;; - website :: <https://github.com/smihica/emmet-mode>
;; - reference ::
;; - <https://github.com/smihica/emmet-mode/blob/master/README.md#usage>

;; NOTE: This package is unmaintained!

(package! emmet-mode
  (setopt emmet-move-cursor-between-quotes t)

  (add-hook 'css-mode-hook #'emmet-mode)
  (after! web-mode
    (add-hook 'web-mode-hook #'emmet-mode)))

;; JavaScript


;; TODO: try <https://github.com/llemaitre19/jtsx> but it's not in melpa or nixpkgs yet

(defun ceamx-init-javascript-modes ()
  (setopt js-indent-level 2)

  (after! lsp-mode
    (lsp-deferred)
    (lsp-lens-mode)
    (dolist (hook '(lsp-format-buffer
                     lsp-organize-imports))
      (add-hook 'before-save-hook hook nil t))))

;; TODO: must happen before `treesit-auto' so it can override
;; (add-to-list 'auto-mode-alist '("\\.js\\'"     . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.pac\\'"    . js2-mode))
;; (add-to-list 'interpreter-mode-alist '("node"  . js2-mode))

(use-feature! typescript-ts-mode
  :init
  (add-hook 'typescript-ts-base-mode #'ceamx-init-javascript-modes))

;; Lua


(use-package lua-mode
  :config
  (setq-default lua-indent-level 2))

;; Customization


(package! markdown-mode
  (setopt markdown-enable-wiki-links t)
  (setopt markdown-italic-underscore t)
  (setopt markdown-asymmetric-header t)
  (setopt markdown-gfm-additional-languages '("sh"))
  (setopt markdown-make-gfm-checkboxes-buttons t)
  (setopt markdown-fontify-whole-heading-line t)

  ;; HACK Due to jrblevin/markdown-mode#578, invoking `imenu' throws a
  ;;      'wrong-type-argument consp nil' error if you use native-comp.
  ;;      <https://github.com/jrblevin/markdown-mode/issues/578>
  (setopt markdown-nested-imenu-heading-index (not (ignore-errors (native-comp-available-p))))

  ;; This is set to `nil' by default, which causes a wrong-type-arg error
  ;; when you use `markdown-open'. These are more sensible defaults.
  (setopt markdown-open-command (cond
                                 ((ceamx-host-macos-p) "open")
                                 ((ceamx-host-gnu-linux-p) "xdg-open")))

  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown))))

(with-eval-after-load 'markdown-mode
  (defvar markdown-mode-map)
  (declare-function markdown-match-generic-metadata "markdown-mode")
  (declare-function markdown-insert-link "markdown-mode")
  (declare-function markdown-insert-blockquote "markdown-mode")

  (define-keymap :keymap markdown-mode-map
    "C-c i l" #'markdown-insert-link
    "C-c i q" #'markdown-insert-blockquote)

  ;; <https://github.com/jrblevin/markdown-mode/issues/328#issuecomment-405361296>
  ;; <https://github.com/radian-software/radian/blob/b2fac3a615186f77de0bdc7e4f06e9aa46c222bb/emacs/radian.el#L3199-L3206>.
  (def-advice! +markdown-disable-front-matter-fontification-a (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block."
    (ignore (goto-char (point-max)))))

;; Install and configure ~nix-mode~ :package:

;; <https://github.com/NixOS/nix-mode>

;; NOTE: ~nix-mode~ should not be loaded when using ~nix-ts-mode~.


(package! nix-mode
  (when (eq 'eglot ceamx-lsp-client)
    (add-hook 'nix-mode-hook #'eglot-ensure))
  (when (eq 'lsp-mode ceamx-lsp-client)
    (add-hook 'nix-mode-hook #'lsp-deferred)))

;; Install and configure ~nix-ts-mode~ :package:

;; <https://github.com/remi-gelinas/nix-ts-mode>


(package! nix-ts-mode
  (when (eq 'eglot ceamx-lsp-client)
    (add-hook 'nix-ts-mode-hook #'eglot-ensure))
  (when (eq 'lsp-mode ceamx-lsp-client)
    (add-hook 'nix-ts-mode-hook #'lsp-deferred)))

;; Set the official formatter (=nixfmt=) as the default formatter


(after! reformatter
  (reformatter-define nixfmt-format
    :group 'ceamx
    :program "nixfmt")

  (add-hook 'nix-mode-hook #'nixfmt-format-on-save-mode)
  (add-hook 'nix-ts-mode-hook #'nixfmt-format-on-save-mode))

(with-eval-after-load 'apheleia
  (add-to-list 'safe-local-variable-values '(apheleia-formatter . nixfmt))
  (add-to-list 'apheleia-mode-alist '(nix-mode . nixfmt))
  (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt)))

;; Register =alejandra= as an additional formatter


(after! reformatter
  (reformatter-define alejandra-format
    :group 'ceamx
    :program "alejandra"))

(with-eval-after-load 'apheleia
  (add-to-list 'safe-local-variable-values '(apheleia-formatter . alejandra))
  (add-to-list 'apheleia-formatters '(alejandra "alejandra")))

;; Configure Nix language servers :lsp:


;; TODO: defcustom
(defvar ceamx-lsp-server-nix-lang "nix-nixd")

(defvar ceamx-lsp-nix-nixd-default-config
  `(:nixpkgs (:expr "import (builtins.getFlake \"/etc/nix/inputs/nixpkgs\") { } ")
    :formatting (:command ["nixfmt"])
    :options (:nixos (:expr ,(format "import (builtins.getFlake \"%s\").%s.\"%s\".options"
                              "/etc/nixos"
                              "nixosConfigurations"
                              (system-name)))
              :home-manager (:expr ,(format "import (builtins.getFlake \"%s\").%s.%s.config.home-manager.users.%s"
                                     "/etc/nixos"
                                     "nixosConfigurations"
                                     (system-name)
                                     (user-login-name))))))

(require 'lib-prog)

(add-to-list 'ceamx-eglot-server-configurations-alist '("nix-nil" . nil))
(add-to-list 'ceamx-eglot-server-configurations-alist
             (cons "nix-nixd" ceamx-lsp-nix-nixd-default-config))

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)

  (add-to-list 'eglot-server-programs
               (cons '(nix-mode nix-ts-mode)
                     (ceamx-eglot-server-contact ceamx-lsp-server-nix-lang))))

(after! lsp-nix
  (setopt lsp-nix-nil-formatter nil)

  (when (string= "nix-nixd" ceamx-lsp-server-nix-lang)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                      :major-modes '(nix-mode nix-ts-mode)
                      :priority 0
                      :server-id 'nixd))))

;; Install ~devdocs~ Nix docset :docs:


(require 'lib-prog)

(def-hook! +devdocs-install-nix-docs ()
  '(nix-mode-hook nix-ts-mode-hook)
  "Install `devdocs' documents for the Nix language."
  (+devdocs-maybe-install "nix"))

;; Keybindings :keybinds:


(after! nix-mode
  (keymap-set nix-mode-map "C-:" #'nix-repl))

(after! (nix-mode tempel)
  (tempel-key "C-c i t a" modargs nix-mode-map))

(after! nix-ts-mode
  (keymap-set nix-ts-mode-map "C-:" #'nix-repl))

(after! nix-repl
  (keymap-set nix-repl-mode-map "C-:" #'quit-window))

;; Feature Settings


(defconst ceamx-lang-php-extension-regexp "\\.\\(php\\|phtml\\)\\'"
  "Pattern matching files with PHP syntax.")

;; Ignore PHP-specific directories and files


(appendq! xref-ignored-files
          '("_ide_helper_models.php"
            "_ide_helper.php"))

;; [[https://github.com/emacs-php/php-mode][emacs-php/php-mode]] :package:


(package! php-mode
  ;; PHP is not a templating language and it never was.  `web-mode' does
  ;; templates better.  Furthermore, as of <2024-06-18 Tue 22:37> `php-ts-mode'
  ;; does not currently support embedded HTML syntax, so disabling this feature
  ;; of `php-mode' adds consistency.
  (setopt php-mode-template-compatibility nil))

;; Display line numbers in PHP buffers


(add-hook 'php-mode-hook #'display-line-numbers-mode)

;; [[https://github.com/Fuco1/neon-mode][Fuco1/neon-mode]]: major-mode for NEON, the PHP-centric franken-YAML DSL :package:

;; - Reference :: <https://ne-on.org/>

;; I have never ever encountered NEON in any context other than a =phpstan.neon=
;; PHPStan configuration file.  This kind of bizarre decision perpetuates PHP's
;; terrible backwards ecosystem.  All that said, I very much appreciate that
;; <Fuco1> has provided this package!  Reality strikes again.


(package! neon-mode)

;; Debugging with Xdebug and ~dap-mode~

;; + ref :: <https://emacs-lsp.github.io/dap-mode/page/configuration/#php>

;; Requires:

;; + [[https://github.com/xdebug/vscode-php-debug?tab=readme-ov-file][GitHub - xdebug/vscode-php-debug: PHP Debug Adapter for Visual Studio Code]]


(after! (:or php-mode phps-mode php-ts-mode)
  (when (featurep 'dap)
    (require 'dap-php)))

;; [[https://github.com/emacs-php/phpstan.el][emacs-php/phpstan.el]]: Provide PHPStan checks :linter:package:


(package! flycheck-phpstan
  (def-hook! +php-mode-load-flycheck-phpstan-h ()
    '(php-mode-hook php-ts-mode-hook)
    "Load the Flycheck checker for PHPStan in PHP buffers."
    (require 'flycheck-phpstan)))

;; =ecs=


(after! reformatter
  (reformatter-define php-ecs-fmt
    :group 'ceamx
    :program (format "%s/vendor/bin/ecs" (getenv "PRJ_ROOT"))
    ;; XXX: Flags are very broken upstream <https://github.com/easy-coding-standard/easy-coding-standard/issues/213>
    :args `("check" "--no-ansi" "--no-progress-bar" "--no-interaction" "--quiet" "--fix" "--" ,input-file)
    :stdin nil
    :stdout nil))

;; =php-cs-fixer=


;; php-cs-fixer pretends to be a patch-output-friendly formatter, but there's no
;; way to stop it from outputting extraneous garbage.  and "--quiet" literally
;; silences everything, even the diff output.  based on similar output issues
;; with ECS (and identically-named flags), i suspect this the fault of the
;; Symfony Console library, which is unfortunately quite widely used.
(after! reformatter
  (reformatter-define php-cs-fixer-fmt
    :group 'ceamx
    :program (format "%s/vendor/bin/php-cs-fixer" (getenv "PRJ_ROOT"))
    :args `("fix" "--using-cache=no" "--sequential" "--no-interaction"
            "--" ,input-file)
    ;; FIXME: symfony/console yet again
    ;; :args `("fix" "--diff" "--using-cache=no" "--show-progress=no"
    ;;         ,(format "--config=%s/%s"
    ;;                  (getenv "PRJ_ROOT")
    ;;                  (or (and (boundp 'ceamx-php-cs-fixer-config-file-path)
    ;;                           ceamx-php-cs-fixer-config-file-path)
    ;;                      ".php-cs-fixer.php"))
    ;;         "--show-progress=none"
    ;;         "-")
    :stdin nil
    :stdout nil))

;; =phpcbf=


(defun +reformatter--phpcbf-fmt-exit-code-success-p (exit-code)
  "Handle PHPCBF non-standard exit codes."
  (or (= 0 exit-code)
      (= 1 exit-code)))

;; FIXME: phpcbf is really finicky and doesn't play nice with the usual
;; formatter standards.
;; + the exit codes are nonsense -- if there are any unfixed errors left in
;;   the file, phpcbf will still return non-zero.
;; + i wonder if the stupid exit codes mean that the patch/diff is output to
;;   stderr instead of stdout as expected by `reformatter-define'?
(after! reformatter
  (reformatter-define phpcbf-fmt
    :program (format "%s/vendor/bin/phpcbf" (getenv "PRJ_ROOT"))
    :args (list "--stdin-path" input-file
                "-q"
                "-")
    ;; XXX: apparently `:exit-code-success-p' does not really accept a lambda? maybe report upstream?
    ;; :exit-code-success-p +reformatter--phpcbf-fmt-exit-code-success-p
    ))

;; Projectile integration


(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

;; TODO ~ceamx-php-wordpress-spaghetti-template-p~: legacy theme template detection


;; (defconst ceamx-php-wordpress-template-names-regexp
;;   (rx))

;; (defun ceamx-php-wordpress-spaghetti-template-p (file)
;;   "Whether a file matches the pattern for a WordPress classic theme template."
;;   )

;; Register Twig (=*.twig=) and Blade (=*.blade.php=) templates to open in ~web-mode~


(after! web-mode
  ;; Blade: Override the default engine in case `web-mode' is associated with the php extension by default.
  (add-to-list 'web-mode-engines-alist '("blade"  . "\\.blade\\."))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php'" . web-mode))
  ;; Twig
  (add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode)))

;; Shell scripts

;; Make sure ~flycheck-mode~ is not enabled in shell script buffers, as
;; ~flymake~ will handle it just fine.


(use-feature! emacs
  :config
  ;; Make files executable if their first line has a shebang.
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(after! eglot
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

(let ((lsp-fn (if (eq 'lsp-mode ceamx-lsp-client)
                  #'lsp-deferred
                #'eglot-ensure)))
  (add-hook 'sh-mode-hook lsp-fn)
  (add-hook 'bash-ts-mode-hook lsp-fn))

(use-feature! flymake
  :config
  (add-hook 'sh-mode-hook #'flymake-mode)
  (add-hook 'bash-ts-mode-hook #'flymake-mode))

;; ~apache-mode~: Language support for Apache Web Server configuration files


(use-package apache-mode
  ;; :ensure t
  )

;; ~just-mode~: Language support for the Just task runner configuration files


(use-package just-mode
  ;;:ensure t
  )

;; ~vimrc-mode~: Language support for =vimrc= syntax


(package! vimrc-mode
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))

;; ~dotenv-mode~: Language support for Dotenv environment configuration files :package:major_mode:

;; - src :: <https://github.com/preetpalS/emacs-dotenv-mode>


(package! dotenv-mode)

(provide 'ceamx-init-langs)
;;; ceamx-init-langs.el ends here
