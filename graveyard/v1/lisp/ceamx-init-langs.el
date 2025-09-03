;; -*- lexical-binding: t -*-

(require 'treesit)

(require 'ceamx-lib)
(require 'ceamx-editor)

;; Programming modes
;; :PROPERTIES:
;; :ID:       c7874765-8cf8-4cf4-abb5-97c220769a65
;; :END:


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

;; General customizations for ~outline-mode~ and ~outline-minor-mode~
;; :PROPERTIES:
;; :ID:       e4910a42-68bf-4549-8384-18fd9dcfd9e4
;; :END:

;; Remember that ~org-mode~ is derived from ~outline-mode~.


(keymap-global-set "<f9>" #'outline-minor-mode)

(after! outline
  (setopt outline-minor-mode-highlight t)
  (setopt outline-minor-mode-cycle t)
  (setopt outline-minor-mode-use-buttons nil))

;; =outli= :: mode-agnostic comment-based outlines

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

;; =smart-newline= :: A self-aware newline command for programming modes :package:


(use-package smart-newline
  ;; :ensure t
  :hook (prog-mode . smart-newline-mode))

;; =dumb-jump= :: Multi-lang do-what-i-mean jump-to-definition :package:

;; - Package :: <https://github.com/jacktasia/dumb-jump>


(package! dumb-jump
  ;; Add to end of `xref-backend-functions' as a dumb fallback when
  ;; there are no smart options.
  ;; FIXME: misuse of `add-hook'
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100)

  (after! (hydra)
    ;; via <https://github.com/jacktasia/dumb-jump?tab=readme-ov-file#hydra-for-effieciency>
    (defhydra ceamx/dumb-jump-dispatch (:color blue :columns 3)
      "Jump (dumbly)"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back"))))

;; =rainbow-mode= :: Colorize color names and hexcodes in buffers :theme:

;; <https://elpa.gnu.org/packages/rainbow-mode.html>


(package! rainbow-mode)

;; =prism= :: Disperse structural forms into depth-based color strata :package:


(use-package prism
  :hook (((python-mode yaml-mode yaml-ts-mode) . prism-whitespace-mode))
  :config
  (setopt prism-num-faces 16)
  ;; TODO: handle `ef-themes'
  (after! modus-themes
    (prism-set-colors
      :desaturations '(0)               ; do not change
      :lightens '(0)                    ; do not change

      ;; 4-color:
      ;; :colors (modus-themes-with-colors
      ;;           (list blue
      ;;                 magenta
      ;;                 magenta-cooler
      ;;                 green-warmer))

      ;; 8-color:
      ;; :colors (modus-themes-with-colors
      ;;           (list blue
      ;;                 magenta
      ;;                 magenta-cooler
      ;;                 cyan-cooler
      ;;                 fg-main
      ;;                 blue-warmer
      ;;                 red-cooler
      ;;                 cyan))

      ;; 16-color:
      :colors (modus-themes-with-colors
                (list fg-main
                      magenta
                      cyan-cooler
                      magenta-cooler
                      blue
                      magenta-warmer
                      cyan-warmer
                      red-cooler
                      green
                      fg-main
                      cyan
                      yellow
                      blue-warmer
                      red-warmer
                      green-cooler
                      yellow-faint)))))

;; =hl-todo= :: Highlight "TODO" and other codetags in comments and strings :package:

;; - website :: <https://github.com/tarsius/hl-todo>
;; - reference :: <https://peps.python.org/pep-0350/#specification>


(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; =indent-bars= :: Display indentation guide-bars


(package! indent-bars
  (dolist (mode '(python-base-mode yaml-mode yaml-ts-mode))
    (add-hook mode #'indent-bars-mode))

  (setopt indent-bars-no-descend-lists t)
  (setopt indent-bars-treesit-support t
          indent-bars-treesit-ignore-blank-lines-types '("module")))

;; =devdocs= :: Peruse <devdocs.io> docsets locally :help:

;; - Source code :: <https://github.com/astoff/devdocs.el>

;; NOTE: Must run ~devdocs-install~ before a docset is available for reference.


(use-package devdocs
  :defer t

  :init
  (setq! devdocs-window-select t)
  (after! popper
    ;; FIXME: no effect -- maybe the package is overriding?
    (add-to-list 'popper-reference-buffers "\\*devdocs\\*"))

  :config
  ;; FIXME: on a stale timer! every week! not every session...
  ;; (devdocs-update-all)

  )

;; Display multiple composed messages inside ~eldoc~ :help:


(setopt eldoc-documentation-function #'eldoc-documentation-compose)



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


(use-feature! jinx
  :hook ((text-mode . jinx-mode))
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)
   :map ceamx-toggle-prefix
   ("s" . jinx-mode))
  :custom
  (jinx-languages "en"))

;; =emacs-reformatter= :: a simple formatter factory :package:
;; :PROPERTIES:
;; :END:


(package! reformatter
  (require 'reformatter))

;; =apheleia= :: an opinionated auto-formatter :package:
;; :PROPERTIES:
;; :END:

;; In case you run into issues with ~web-mode~ not updating syntax highlighting after
;; formatting (or other arbitrary modifications):
;; <https://github.com/doomemacs/doomemacs/blob/35dc13632b3177b9efedad212f2180f69e756853/modules/editor/format/config.el#L74-L83>


(package! apheleia)

(after! (apheleia minions)
  (add-to-list 'minions-prominent-modes #'apheleia-mode))

;; =prettier=
;; :PROPERTIES:
;; :ID:       5a8a7b6e-09cc-4f48-b6a6-3a9a12a41018
;; :END:

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

;; Use Biome as formatter in supported major modes :lang:formatting:checkers:


(after! reformatter
  (reformatter-define biome-format
    :program "biome"
    :args (list "format" "--stdin-file-path" (buffer-file-name)))

  (dolist (hook (ceamx-editor-biome-supported-modes-hooks))
    (add-hook hook #'biome-format-on-save-mode)))

(after! apheleia
  (add-to-list 'apheleia-formatters '(biome "biome" "format" "--stdin-file-path" filepath))

  (dolist (mode ceamx-editor-biome-supported-modes-list)
    (add-to-list 'apheleia-mode-alist '(mode . biome))))

;; Define user option to disable format-on-save for some modes :formatting:


(defcustom ceamx-format-on-save-disabled-modes
  '(emacs-lisp-mode                     ; conflict with `lispy' indent
    org-msg-edit-mode)
  "A list of major modes in which to not reformat the buffer upon saving.
When nil, buffers will always be formatted upon save. When
non-nil, buffers will never be formatted upon save."
  :group 'ceamx
  :type '(choice boolean (repeat symbol)))

;; Inhibit automatic formatting in some contexts :formatting:

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

;; Inhibit on-save formatting with prefix argument :formatting:

;; - Source :: <https://github.com/radian-software/radian/blob/20c0c9d929a57836754559b470ba4c3c20f4212a/emacs/radian.el#L2266-L2270>


(after! apheleia
  (def-advice! +apheleia-save-buffer-maybe-reformat-a (func &optional arg)
    :around #'save-buffer
    "Inhibit reformatting-on-save when providing a prefix argument to \\[save-buffer]."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func))))

;; Linting files with the builtin ~flymake~ feature :flymake:
;; :LOGBOOK:
;; - Refiled on [2025-07-23 Wed 12:34]
;; :END:


(after! flymake
  (setq! flymake-fringe-indicator-position 'right-fringe)
  (setq! flymake-no-changes-timeout 1.0)
  (setq! flymake-wrap-around t))

;; =flycheck= :: The /other/ file diagnostics provider :package:
;; :LOGBOOK:
;; - Refiled on [2025-07-23 Wed 12:34]
;; :END:


(package! flycheck
  (add-hook 'ceamx-after-init-hook #'global-flycheck-mode))

(package! consult-flycheck
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

;; =puni= :: versatile structural editing

;; <https://github.com/AmaiKinono/puni>


(package! puni
  (puni-global-mode)
  (add-hook 'prog-mode-hook #'puni-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;; =treesit-auto= :: automatically use available ~treesit~ modes

;; + Package :: <https://github.com/renzmann/treesit-auto>

;; NOTE: This package does *not* automatically manage mode-hook
;; translation.  Those should be managed manually on a case-by-case
;; basis. For example, ~nix-ts-mode-hook~ does not currently inherit the
;; value of ~nix-mode-hook~.  Some Tree-Sitter modes, however, still derive
;; from their non-Tree-Sitter predecessor, and so will also run that
;; mode's hooks in addition to its own.

;; NOTE: This feature is intended to be loaded *after* all other language
;; packages have been installed so that ~treesit-auto~ it can override
;; ~auto-mode-alist~.

;; By default, Emacs plays it safe with tree-sitter language support so
;; as not to override legacy mode file extension associations.  This
;; makes sense as a default, but it's a pain to have to override
;; ~auto-mode-alist~ for every language individually.

;; ~treesit-auto~ is pretty smart about how it handles these behaviors; its
;; readme provides more in-depth details.

;; In short, ~global-treesit-auto-mode~ will:

;; - Automatically switch to <name>-ts-mode when the grammar for <name>
;;   is installed
;; - Stick with <name>-mode if the grammar isn’t installed
;; - Automatically install a grammar before opening a compatible file
;; - Modify auto-mode-alist for tree-sitter modes

;;   See also
;;   <https://github.com/purcell/emacs.d/blob/master/lisp/init-treesitter.el>
;;   for a more manual approach.


(package! treesit-auto
  (require 'treesit-auto)

  ;; Grammars should be installed via Nixpkgs.
  (setopt treesit-auto-install nil)

  (treesit-auto-add-to-auto-mode-alist 'all)

  (global-treesit-auto-mode))



;; Increase the amount of syntax-highlighted structures:


(setopt treesit-font-lock-level 4)

;; =treesit-fold= :: Code folding with ~treesit~ :folding:

;; + Package :: <https://github.com/emacs-tree-sitter/treesit-fold>

;;   There is a lot to configure…


(package! treesit-fold
  (setopt treesit-fold-line-count-show t)
  (setopt treesit-fold-summary-show t)

  (global-treesit-fold-mode 1)
  (global-treesit-fold-indicators-mode -1))

;; =combobulate= :: A consistent structural navigation interface
;; :LOGBOOK:
;; - Refiled on [2025-01-26 Sun 16:39]
;; :END:


(package! (combobulate :host github :repo "mickeynp/combobulate")
  (add-hook 'prog-mode-hook #'combobulate-mode)

  (setopt combobulate-key-prefix "C-c l o"))

;; Apply ~autoinsert~ skeletons to new files


(use-feature! autoinsert
  :config
  (auto-insert-mode 1))

;; General LISPs

;; Configuration for working with Lisps of all kinds.


(require 'derived)
(require 'ceamx-lisp)

;; Configure behavior for all Lisp modes with ~ceamx-lisp-init-hook~
;; :PROPERTIES:
;; :ID:       6070743a-a471-43f6-9f99-6f117b81ce95
;; :END:


(add-hook 'ceamx-lisp-init-hook #'ceamx-enable-check-parens-on-save)

;; Add hooks to supported Lisp modes.
(dolist (mode ceamx-lisp-modes-list)
  (add-hook (derived-mode-hook-name mode) #'ceamx-lisp-init))

;; ~lispy~ :: the structural expression editing experience

;; - Website :: [[https://github.com/abo-abo/lispy][GitHub - abo-abo/lispy: Short and sweet LISP editing]]
;; - API Reference :: [[https://oremacs.com/lispy/][lispy.el function reference]]


(package! lispy
  (add-hook 'ceamx-lisp-init-hook #'lispy-mode))

(after! lispy
    (setopt lispy-completion-method 'default)
    (setopt lispy-eval-display-style 'message)
    (setopt lispy-move-after-commenting t)

    (define-keymap :keymap lispy-mode-map
      "M-j" nil                         ; shadows custom binding
      "M-o" nil                         ; shadows custom binding
      ;; via <https://github.com/abo-abo/lispy/pull/619>
      "`" #'self-insert-command)

    (after! outli
      ;; `outli-mode' overrides `lispy-mode' outline functionality, so
      ;; it must be activated afterwards.
      (add-hook 'ceamx-lisp-init-hook #'outli-mode))

    (after! macrostep
      (push 'macrostep lispy-compat))

    (after! popper
      (push "\\*lispy-message\\*" popper-reference-buffers)))

;; =kbd-mode= :: syntax support for =kmonad= and =kanata= configs

;; + Package :: [[https://github.com/kmonad/kbd-mode][GitHub - kmonad/kbd-mode: Emacs mode for syntax highlighting kmonad's .kbd files.]]


(package! (kbd-mode :host github :repo "kmonad/kbd-mode"))



;; Unfortunately, we need to inhibit formatters because whitespace is used to convey
;; non-syntactic meaning to the reader.


(after! kbd-mode
  (add-to-list 'ceamx-format-on-save-disabled-modes #'kbd-mode)
  (after! lispy
    (add-to-list 'lispy-no-indent-modes #'kbd-mode)))

;; General Elisp support customizations


(defun ceamx-emacs-lisp-init ()
  "Sensible defaults for `emacs-lisp-mode'."
  (ceamx-lisp-init)
  (eldoc-mode 1))

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

;; (keymap-global-set "<remap> <indent-pp-sexp>" #'ceamx/indent-last-sexp)

(define-keymap :keymap emacs-lisp-mode-map
  "C-:" #'ielm

  "C-S-t" #'transpose-sexps)

(with-eval-after-load 'ielm
  (defvar ielm-map)
  (keymap-set ielm-map "C-:" #'quit-window))

;; =eros= :: [E]valuation [R]esult [O]verlay[S]
;; :PROPERTIES:
;; :ID:       a59594e9-edbc-4446-9f2f-6f00b34c8034
;; :END:

;; + Website :: <https://github.com/xiongtx/eros>


(package! eros
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

;; =suggest= :: meet the elisp function of your dreams

;; + Package :: <https://github.com/Wilfred/suggest.el>


(use-package suggest
  :commands (suggest)
  :init
  (keymap-set emacs-lisp-mode-map "C-c s" #'suggest))

;; =macrostep= :: interactive macro-explorer
;; :PROPERTIES:
;; :ID:       66579505-69d5-4c9c-bacb-12b27ffb958a
;; :END:

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

;; =xr= :: convert string regexps to ~rx~ forms

;; + Package :: <https://github.com/mattiase/xr>


(package! xr)

;; =elmacro=: Display keyboard macros or latest interactive commands as Elisp

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

;; =elisp-demos= :: display elisp usage examples inside help buffers

;; - Package :: <https://github.com/xuchunyang/elisp-demos>


(use-package elisp-demos
  ;; :ensure t
  :defer 5
  :after (helpful)
  :functions (elisp-demos-advice-helpful-update)
  :config
  (setopt elisp-demos-user-files (list (expand-file-name  "docs/elisp-demos.org" user-emacs-directory)))

  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; =morlock= :: more font-lock keywords for some eldritch expressions

;; + Package :: <https://github.com/tarsius/morlock>


(package! morlock
  (add-hook 'ceamx-after-init-hook #'morlock-mode))

;; =keymap-utils= :: dev library for working with keymaps


(package! keymap-utils)

;; =eglot= :: Eglot :eglot:


(after! eglot
  (setopt eglot-sync-connect 1)
  (setopt eglot-autoshutdown t)
  (setopt eglot-send-changes-idle-time 0.5)

  ;; Disable events buffer, which poses performance issues over time as the
  ;; buffer grows in a longer-running Emacs instance.
  (setopt eglot-events-buffer-size 0)

  ;; Prevent frequent focus-stealing.
  (setopt eglot-auto-display-help-buffer nil))

;; =eglot-booster= :: Handler for =emacs-lsp-booster= :perf:

;; - Website :: <https://github.com/jdtsmith/eglot-booster>
;; - Website :: <https://github.com/blahgeek/emacs-lsp-booster>

;; Requires =emacs-lsp-booster= to be installed into the environment.
;; Available by that name in Nixpkgs.


(package! (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (after! eglot
    (eglot-booster-mode)))



;; Though I have not tried it, I am thinking that using =lsp-booster= over TRAMP is
;; not worth the trouble of ensuring that the executable is available on every
;; remote server.  At least not as a default behavior.  Consider enabling this
;; per-project or server as desired.


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

;; Declare some Eglot buffers as popup windows :popups:


(after! (eglot popper)
  (defvar popper-reference-buffers)
  (add-to-list 'popper-reference-buffers "^\\*eglot-help"))

;; Enable JSON schema validation via the SchemaStore catalog :json:
;; :PROPERTIES:
;; :ID:       aa0c0d8b-a74e-4064-b749-519e16af0999
;; :END:


(use-feature! ceamx-eglot
  :demand t
  :after eglot
  :defines (ceamx-eglot-server-configurations-alist)
  :config
  (let ((schemata (ceamx-eglot-json-schema-catalog)))
    (setq-default
     eglot-workspace-configuration
     (map-insert eglot-workspace-configuration
                 :json ; <https://github.com/microsoft/vscode/blob/main/extensions/json-language-features/server/README.md>
                 `( :validate (:enable t)
                    :schemas ,schemata
                    :resultLimit 10000
                    :initializationOptions ( :handledSchemaProtocols ["file" "https"]))))
    (setq-default
     eglot-workspace-configuration
     (map-insert eglot-workspace-configuration
                 :yaml ; <https://github.com/redhat-developer/yaml-language-server/blob/main/README.md>
                 `( :validate (:enable t)
                    :schemas ,schemata)))))

;; =flycheck-eglot= :: Eglot-Flycheck integration :flycheck:


(package! flycheck-eglot
  (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode))

;; =consult-eglot= :: Add Eglot workspace symbols as Consult datasource :consult:

;; <https://github.com/mohkale/consult-eglot>


(package! consult-eglot
  (defalias 'ceamx/list-workspace-symbols #'consult-eglot-symbols))

;; Use Biome language server in supported modes :biome:checkers:formatting:lsp:

;; - Docs :: <https://biomejs.dev/guides/integrate-in-editor/>
;; - Reference :: <https://biomejs.dev/internals/language-support/>


(use-feature! ceamx-eglot
  :demand t
  :after eglot
  :defines (ceamx-eglot-server-configurations-alist)
  :config
  ;; FIXME: only start server if "biome.json" file in tree
  (add-to-list 'eglot-server-programs
               (list ceamx-editor-biome-supported-modes-list "biome" "lsp-proxy")))

;; JSON/JSONC


(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-ts-mode))

;; Register =taplo= formatting utilities :formatting:


(after! reformatter
  (reformatter-define toml-taplo-fmt
    :group 'ceamx
    :program "taplo"
    :args (list "format" "--diff"
                "--stdin-filepath" (buffer-file-name)
                "-"))

  (add-hook 'conf-toml-mode-hook #'toml-taplo-fmt-on-save-mode)
  (add-hook 'toml-ts-mode-hook #'toml-taplo-fmt-on-save-mode))

;; Use the =taplo= language server :eglot:lsp:


(use-feature! ceamx-eglot
  :demand t
  :after eglot
  :defines (ceamx-eglot-server-configurations-alist)

  :config
  (add-to-list 'ceamx-eglot-server-configurations-alist
               '("toml-taplo" . nil))
  (add-to-list 'eglot-server-programs
               (cons '(conf-toml-mode toml-ts-mode)
                     (ceamx-eglot-server-contact
                      "toml-taplo"
                      "taplo" "lsp" "stdio"))))

;; Install the =yaml-mode= package


(package! yaml-mode)

;; =yaml-pro= :: Because YAML is a terrible

;; + Usage :: https://github.com/zkry/yaml-pro#usage


(package! yaml-pro
  (after! yaml-mode
    ;; Why you would use this mode when `treesit' is available is
    ;; currently a mystery to me, but anything can happen (I have seen
    ;; `php-ts-mode' break before)...
    (when (treesit-ready-p 'yaml)
      (add-hook 'yaml-mode-hook #'yaml-pro-ts-mode 100)))
  (after! yaml-ts-mode
    (add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode 100)))

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

;; KDL


(package! (kdl-ts-mode :host github :repo "merrickluo/kdl-ts-mode")
  ;; TODO: I'm not sure why this wasn't done automatically -- it's in the
  ;; `kdl-ts-mode' file.  But I guess the file isn't loaded, so there's
  ;; no way for Emacs to know.  How is this normally done without
  ;; requiring user intervention?
  (add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode)))

;; =jq-mode= :: major mode for editing =jq= queries
;; :PROPERTIES:
;; :ID:       14cef8c8-c0c9-4110-839b-4eb7c5e3da96
;; :END:

;; + Package :: <https://github.com/ljos/jq-mode>


(package! jq-mode
  (add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))

  ;; HACK: Prevent duplicate paren insertion.
  ;;       <https://github.com/ljos/jq-mode/issues/44>
  (add-hook 'jq-mode-hook (##electric-pair-local-mode -1))

  (after! json
    (keymap-set js-json-mode-map "C-:" #'jq-interactively))
  (after! json-ts-mode
    (keymap-set json-ts-mode-map "C-:" #'jq-interactively)))

;; JavaScript


;; TODO: try <https://github.com/llemaitre19/jtsx>

(defun ceamx-init-javascript-modes ()
  (setopt js-indent-level 2)

  (when (fboundp 'lsp-mode)
    (lsp-deferred)
    (lsp-lens-mode)
    ;; FIXME: defer to biome cli or use its lsp server?
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

;; Rust


(package! rust-mode)

(package! rustic
  (setq! rustic-format-trigger nil
          rustic-babel-format-src-block nil)
  (setq! rustic-cargo-use-last-stored-arguments t))

;; (after! (rust-mode rustic)
;;   (map-put 'auto-mode-alist '("\\.rs\\'" . rust-mode))
;;   (map-put 'auto-mode-alist '("\\.rs\\'" . rustic-mode)))

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


(package! nix-mode)

;; Install and configure ~nix-ts-mode~ :package:

;; <https://github.com/remi-gelinas/nix-ts-mode>


(package! nix-ts-mode)

(after! (nerd-icons nix-ts-mode)
  ;; XXX: contribute fix upstream
  (add-to-list 'nerd-icons-mode-icon-alist '(nix-ts-mode  nerd-icons-mdicon "nf-md-nix" :face nerd-icons-blue)))

;; Set the official formatter (=nixfmt=) as the default formatter :formatting:


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

;; Register =alejandra= as an additional formatter :formatting:


(after! reformatter
  (reformatter-define alejandra-format
    :group 'ceamx
    :program "alejandra"))

(with-eval-after-load 'apheleia
  (add-to-list 'safe-local-variable-values '(apheleia-formatter . alejandra))
  (add-to-list 'apheleia-formatters '(alejandra "alejandra")))

;; Configure Nix language servers :lsp:


(after! nix-mode
  (if (fboundp 'lsp-mode)
      (add-hook 'nix-mode-hook #'lsp-deferred)
    (add-hook 'nix-mode-hook #'eglot-ensure)))

(after! nix-ts-mode
  (if (fboundp 'lsp-mode)
      (add-hook 'nix-ts-mode-hook #'lsp-deferred)
    (add-hook 'nix-ts-mode-hook #'eglot-ensure)))

(after! lsp-mode
  (setopt lsp-disabled-clients '(nix-nixd))
  (setopt lsp-nix-nil-formatter nil
          lsp-nix-nil-max-mem 10000
          lsp-nix-nil-auto-eval-inputs t
          lsp-nix-nil-ignored-diagnostics nil
          lsp-nix-nil-exclude-files-diagnostic nil))

(use-feature! ceamx-eglot
  :demand t
  :after eglot
  :defines (ceamx-eglot-server-configurations-alist)
  :config
  (add-to-list 'ceamx-eglot-server-configurations-alist '("nix-nil" . nil)))

;; Automatically insert a semicolon after contextual character sequences :edit:

;; This is a game-changer and huge motion-saver.  Without this, I was
;; always moving up and down lines to add semicolons every time I added new
;; braces, which happens a lot in Nix.

;; FIXME: Note that the equals-space (“= ”) sequence does not trigger insertion as
;; it should!


(defun ceamx-char-next (&optional pos)
  "Next non-space character after POS within a three-character range.
POS defaults to point.  POS is an integer or a marker as in
`char-after', which see.

Examples of matching behavior, where each string represents the
three characters immediately following point:

  \"abc\" => \"a\"
  \"a c\" => \"a\"
  \" bc\" => \"b\"
  \"  c\" => \"c\"
  \"   \" => \" \"
  \"a b\" => \"a\"
"
  (let* ((position (or pos (point)))
         (next (char-after position)))
    (if (or (char-equal ?\s next)
            (char-equal ?\s (char-after next)))
        (char-after next)
      next)))

(defun ceamx-nix-insert-semicolon-after-sequence ()
  "Insert a semicolon after relevant characters in `nix-ts-mode'.

Closing square brackets, closing curly brackets, and the equals sign are
handled, while parens are ignored.  This behavior is compatible with
`electric-pair-mode'.

The following character sequences will allow semicolon insertion:

 - Immediately after a single one of: right square bracket, right curly
   bracket; for example: \"}\"

 - After a single space preceded by one of: equals sign; for example:
   \"= \"

A semicolon will not be inserted under the following conditions:

 - Point is at end of buffer or, by extension, the bracket(s) are the
   top-level form in the buffer.

 - The closing bracket precedes a semicolon, left brace, any paren, or
   any square bracket separated by zero to two spaces."
  (when (and (eq major-mode #'nix-ts-mode)
             (or (memq (char-before (point)) '(?\} ?\]))
                 (and (eq (char-before (point)) ?\s)
                      (memq (char-before (char-before (point))) '(?=))))
             (not (memq (ceamx-char-next) '(?\; ?\{ ?\) ?\( ?\[ ?\])))
             (not (eq (point) (point-max))))
    (insert ";")
    (backward-char)))



;; For the automatic behavior, the function will need to be hooked into
;; ~post-self-insert-hook~:


(add-hook 'post-self-insert-hook #'ceamx-nix-insert-semicolon-after-sequence)

;; Keybindings :keybinds:
;; :PROPERTIES:
;; :ID:       108e009a-acd4-434a-9eb5-448ba94ebb77
;; :END:


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

;; =php-ts-mode= [builtin]

;; ~php-ts-mode~ is part of Emacs 30.

;; Unfortunately, as of <2024-06-18 Tue 18:22>, I am missing the
;; [[https://github.com/claytonrcarter/tree-sitter-phpdoc][=tree-sitter-phpdoc= grammar]], and it is not yet available in Nixpkgs.  I was
;; able to run ~php-ts-mode-install-parsers~ as suggested in the error message, but
;; the missing grammar should really be added to Nixpkgs and the current stateful
;; installation might get confusing.


(when (and (fboundp 'php-ts-mode)
           (treesit-language-available-p 'php))
  (add-to-list 'major-mode-remap-alist '(php-mode . php-ts-mode))
  (add-to-list 'major-mode-remap-alist '(php-mode-maybe . php-ts-mode)))

;; Display line numbers in PHP buffers


(after! php-mode
  (add-hook 'php-mode-hook #'display-line-numbers-mode))

(after! php-ts-mode
  (add-hook 'php-ts-mode-hook #'display-line-numbers-mode))

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
  (add-to-list 'eglot-server-programs
      '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

(after! sh-script
  (add-hook 'sh-mode-hook #'eglot-ensure)
  (add-hook 'bash-ts-mode-hook #'eglot-ensure)
  (add-hook 'sh-mode-hook #'flymake-mode)
  (add-hook 'bash-ts-mode-hook #'flymake-mode))

;; =apache-mode= :: Language support for Apache Web Server configuration files


(package! apache-mode)

;; =fish-mode= :: Language support for Fish shell script files

;; + Pkgs :: =fish-lsp=


(package! fish-mode
  ;; 4 spaces is standard according to examples in Fish shell
  ;; documentation.  It's also generally an informal standard for shell
  ;; scripts (unless you're writing shell scripts in YAML).
  (setq! fish-indent-offset 4)

  (after! eglot
    ;; FIXME: displays a pointless confirmation prompt on start.  the
    ;; cli does not appear to provide a way to disable this.  maybe
    ;; should check the upstream issue tracker.  no other language
    ;; server that i have used does this.
    (cl-pushnew '(fish-mode . ("fish-lsp" "start")) eglot-server-programs)))

;; =just-mode= :: Language support for the Just task runner configuration files


(package! just-ts-mode)

;; Register LSP support for the =just-lsp= language server :lsp:eglot:lsp_mode:

;; - Website :: https://github.com/terror/just-lsp

;; Requires that the =just-lsp= package is available in the environment.


(use-feature! ceamx-eglot
  :demand t
  :after eglot
  :defines (ceamx-eglot-server-configurations-alist)
  :config
  (add-to-list 'ceamx-eglot-server-configurations-alist '("just-just-lsp" . nil))
  (add-to-list 'eglot-server-programs
               (cons '(just-mode just-ts-mode)
                     (ceamx-eglot-server-contact "just-just-lsp"))))

(use-feature! ceamx-lsp-just
  :demand t
  :after (lsp-mode just-ts-mode)
  :config
  (add-hook 'just-ts-mode-hook #'lsp-deferred))

;; =vimrc-mode= :: Language support for =vimrc= syntax


(package! vimrc-mode
  (add-to-list 'auto-mode-alist '("\\.(idea)?vim\\(rc\\)?\\'" . vimrc-mode)))

;; =dotenv-mode= :: Language support for Dotenv environment configuration files :package:major_mode:

;; - src :: <https://github.com/preetpalS/emacs-dotenv-mode>


(package! dotenv-mode)

;; =yuck-mode= :: Language support for =yuck= configuration files for ElKowar’s Wacky Widgets


(package! yuck-mode)

;; =web-mode=
;; :PROPERTIES:
;; :ID:       19c081bf-6b29-4fbf-bffb-4e519b94fe45
;; :END:


(package! web-mode
  ;; Ensure these associations with `web-mode' take priority over
  ;; existing associations.  These should be added after other
  ;; packages add their own values to the list.
  (dolist (match '("\\.html?\\'"
                   "\\.blade\\.php\\'"
                   "\\.tpl\\.php\\'"
                   "\\.phtml\\'"
                   "\\.[agj]sp\\'"
                   "\\.as[cp]x\\'"
                   "\\.erb\\'"
                   "\\.mustache\\'"
                   "\\.djhtml\\'"))
    (cl-pushnew (cons match 'web-mode) auto-mode-alist))

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

(provide 'ceamx-init-langs)
;;; ceamx-init-langs.el ends here
