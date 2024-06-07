;;; init-completion.el --- Completion enhancements  -*- lexical-binding: t;  -*-

;; Copyright (c) 2022-2024  Chris Montgomery <chmont@proton.me>

;; Author: Chris Montgomery <chmont@proton.me>
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
;;; Code:

(require 'ceamx-lib)
(require 'lib-completion)
;; Always resize mini-windows to fit their contents.
(setopt resize-mini-windows t)

;; TAB cycle if there are only few candidates
(setopt completion-cycle-threshold 2)

;; Hide commands in M-x which do not apply to the current mode.  Corfu commands
;; are hidden, since they are not used via M-x.  This setting is useful beyond
;; Corfu.
(setopt read-extended-command-predicate #'command-completion-default-include-p)

;; Don't let `completion-at-point' interfere with indentation.
(setopt tab-always-indent 'complete)

;; `completion-at-point' is often bound to M-TAB, but that conflicts with OS behavior.
;; We also want to preserve "C-S-SPC" , the Emacs default binding for `set-mark-command'.
(keymap-global-set "C-S-SPC" #'completion-at-point)
(setopt enable-recursive-minibuffers t)
(setopt read-extended-command-predicate #'command-completion-default-include-p)
(setopt minibuffer-prompt-properties '( read-only t
                                        cursor-intangible t
                                        face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(defvar crm-separator)

(defun +crm-indicator (args)
  "Add prompt indicator to `completing-read-multiple' (ARGS are candidates).
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'+crm-indicator)
(defvar savehist-additional-variables)
(package! vertico
  ;; (setopt vertico-scroll-margin 0)
  (setopt vertico-count 5)
  (setopt vertico-resize t)
  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (setopt vertico-cycle t)

  (vertico-mode))

;; Avy-like candidate selection
(after! vertico
  (keymap-set vertico-map "M-q" #'vertico-quick-insert)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit))
(after! vertico
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index start)
      (setq cand (funcall orig cand prefix suffix index start))
      (concat (if (= vertico--index index)
                  (propertize "» " 'face 'vertico-current)
                "  ")
              cand))))
(after! vertico
  ;; FIXME: why is this disabled?
  ;; via <https://github.com/minad/vertico/wiki#additions-for-moving-up-and-down-directories-in-find-file>
  ;; (defun ceamx/vertico-directory-delete-entry ()
  ;;   "Delete directory or entire entry before point."
  ;;   (interactive)
  ;;   (when (and (> (point) (minibuffer-prompt-end))
  ;;              ;; Check vertico--base for stepwise file path completion
  ;;              (not (equal vertico--base ""))
  ;;              (eq 'file (vertico--metadata-get 'category)))
  ;;     (save-excursion
  ;;       (goto-char (1- (point)))
  ;;       (when (search-backward "/" (minibuffer-prompt-end) t)
  ;;         (delete-region (1+ (point)) (point-max))
  ;;         t))))

  (define-keymap :keymap vertico-map
    "RET"     #'vertico-directory-enter
    "DEL"     #'vertico-directory-delete-char
    "M-DEL"   #'vertico-directory-delete-word)

  ;; Tidy shadowed file names -- e.g. cleans `~/foo/bar///' to `/', and `~/foo/bar/~/' to `~/'.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
(after! (vertico savehist)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-to-list 'savehist-additional-variables #'vertico-repeat-history))
;;   M-B -> `vertico-multiform-buffer'
;;   M-F -> `vertico-multiform-flat'
;;   M-G -> `vertico-multiform-grid'
;;   M-R -> `vertico-multiform-reverse'
;;   M-U -> `vertico-multiform-unobtrusive'
;;   M-V -> `vertico-multiform-vertical'
(after! vertico
  ;; NOTE: Takes precedence over `vertico-multiform-categories'.
  (setopt vertico-multiform-commands
          `((consult-line buffer)
           (consult-imenu reverse buffer)
           (consult-outline buffer quick ,(lambda (_) (text-scale-set -1)))
           (execute-extended-command flat)))

  (setopt vertico-multiform-categories
          '((file buffer grid)
            (buffer flat (vertico-cycle . t))
            (consult-grep buffer)
            (imenu (:not indexed mouse))
            (symbol (vertico-sort-function . vertico-sort-alpha))))

  (vertico-multiform-mode))
(package! consult)
(setopt consult-narrow-key "<") ;; alternatively: "C-+"
(setopt consult-preview-key 'any)
;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
(after! consult
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any)))
(setopt register-preview-delay 0.5)
(setopt register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)
(setopt xref-show-definitions-function #'consult-xref)
(setopt xref-show-xrefs-function #'consult-xref)
(define-keymap :keymap (current-global-map)
  "C-c M-x" #'consult-mode-command
  ;; TODO: needs configuration? see `consult-mode-histories'
  ;; "C-c h"   #'consult-history
  "C-c K" #'consult-kmacro
  "C-c M" #'consult-man

  ;; FIXME: use a keymap for s prefix
  ;; "C-c s h"  #'consult-history
  ;; "C-c s m"  #'consult-mode-command
  ;; "C-c s k"  #'consult-kmacro

  "<remap> <Info-search>" #'consult-info

  "C-x M-:" #'consult-complex-command     ; orig. `repeat-complex-command'
  "C-x b" #'consult-buffer                ; orig. `switch-to-buffer'
  "C-x 4 b" #'consult-buffer-other-window ; orig. `switch-to-buffer-other-window'
  "C-x 5 b" #'consult-buffer-other-frame ; orig. `switch-to-buffer-other-frame'
  "C-x t b" #'consult-buffer-other-tab   ; orig. `switch-to-buffer-other-tab'
  "C-x r"  #'consult-bookmark            ; orig. `bookmark-jump'
  "C-x p b" #'consult-project-buffer     ; orig. `project-switch-to-buffer'

  ;; Custom M-# bindings for fast register access
  "M-#"    #'consult-register-load
  "M-'"    #'consult-register-store
  "C-M-#"  #'consult-register

  ;; Miscellaneous
  "M-y" #'consult-yank-pop            ; orig. `yank-pop'

  ;; M-g bindings (goto-map)
  "M-g e"  #'consult-compile-error
  "M-g f"  #'consult-flymake ;; Alternative: consult-flycheck
  "M-g g" #'consult-goto-line
  "M-g M-g" #'consult-goto-line
  "M-g o"  #'consult-outline ;; Alternative: consult-org-heading
  "M-g m"  #'consult-mark
  "M-g k"  #'consult-global-mark
  "M-g i"  #'consult-imenu
  "M-g I"  #'consult-imenu-multi

  ;; M-s bindings (search-map)
  "M-s d"  #'consult-fd               ; or `consult-find'
  "M-s D"  #'consult-locate
  "M-s e"  #'consult-isearch-history
  "M-s g"  #'consult-ripgrep
  "M-s G"  #'consult-git-grep
  "M-s l"  #'consult-line
  "M-s L"  #'consult-line-multi
  "M-s k"  #'consult-keep-lines
  "M-s u"  #'consult-focus-lines)
(after! isearch
  (define-keymap :keymap isearch-mode-map
    "M-e"   #'consult-isearch-history
    "M-s e" #'consult-isearch-history
    "M-s l" #'consult-line
    "M-s L" #'consult-line-multi))
(keymap-set minibuffer-local-map "M-s" #'consult-history) ; orig. next-matching-history-element
(keymap-set minibuffer-local-map "M-r" #'consult-history)
(after! consult

  ;; Make narrowing help available in the minibuffer.
  ;; FIXME: What if Embark commands are not autoloaded yet?
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'embark-prefix-help-command)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  (with-eval-after-load 'projectile
    (declare-function projectile-project-root "projectile")
    (setopt consult-project-function (lambda (_) (projectile-project-root)))))
(package! embark-consult
  (after! embark
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

  (after! (embark consult)
    (require 'embark-consult)))
(after! (consult consult-imenu pulsar)
  (setq consult-after-jump-hook nil)
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))
(package! marginalia
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

  (marginalia-mode))
(after! (marginalia)
  (when (featurep 'projectile)
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file))))
(package! orderless
  ;; FIXME: unnecessary as long as stuff is configured
  (require 'orderless)

  ;; Allow escaping space with backslash.
  (setopt orderless-component-separator #'orderless-escapable-split-on-space))
;; (defun +orderless-flex-if-twiddle-dispatch (pattern _index _total)
;;   "Return `orderless-flex' if PATTERN ends in a tilde character.
;; PATTERN, stripped of its tilde character, will be dispatched as
;; argument to `orderless-flex'."
;;   (when (string-suffix-p "~" pattern)
;;     `(orderless-flex . ,(substring pattern 0 -1))))

;; (defun +orderless-first-initialism-dispatch (_pattern index _total)
;;   "Return `orderless-initialism' when PATTERN has the initial INDEX value."
;;   (if (= index 0) 'orderless-initialism))

;; (defun +orderless-not-if-bang-dispatch (pattern _index _total)
;;   "Return `orderless-not' when PATTERN begins with an exclamation mark.
;; PATTERN, stripped of its exclamation mark, will be dispatched as
;; argument to `orderless-not'."
;;   (cond
;;    ((equal "!" pattern)
;;     #'ignore)
;;    ((string-prefix-p "!" pattern)
;;     `(orderless-not . ,(substring pattern 1)))))

(defun +orderless--consult-suffix ()
  "Regexp which matches the end of string with Consult tofu support."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
              consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

;; Recognizes the following patterns:
;; + .ext (file extension)
;; + regexp$ (regexp matching at end)
(defun +orderless-consult-dispatch (word _index _total)
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" word)
    `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
    `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))
(after! orderless
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))
(after! orderless
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides '((file (styles partial-completion))
                                          (command (styles +orderless-with-initialism))
                                          (variable (styles +orderless-with-initialism))
                                          (symbol (styles +orderless-with-initialism))))

  (setopt orderless-matching-styles '(orderless-regexp))
  (setopt orderless-style-dispatchers (list ;; #'+orderless-first-initialism-dispatch
                                       ;; #'+orderless-flex-if-twiddle-dispatch
                                       ;; #'+orderless-not-if-bang-dispatch
                                       #'+orderless-consult-dispatch
                                       #'orderless-affix-dispatch)))
(package! corfu
  (setopt corfu-auto t)
  (setopt corfu-auto-delay 0.05)
  (setopt corfu-cycle t)
  (setopt corfu-preselect 'prompt)
  (setopt corfu-count 8)
  (setopt corfu-max-width 80)
  (setopt corfu-on-exact-match nil)
  (setopt corfu-scroll-margin 5)

  (setopt corfu-quit-at-boundary 'separator)
  (setopt corfu-quit-no-match 'separator)

  ;; Trigger insertion of the separator with "M-SPC".  The character will appear
  ;; to be inserted into the buffer.  Upon selecting a candidate (or aborting,
  ;; or whatver), the extra character will be removed.
  (setopt corfu-separator ?_)

  (global-corfu-mode))
(after! corfu
  (define-keymap :keymap corfu-map
    "TAB" #'corfu-next
    "S-TAB"  #'corfu-previous
    "RET" nil
    "C-h" #'corfu-info-documentation
    "C-n" nil
    "C-p" nil
    "C-RET" #'corfu-insert
    "M-." #'corfu-show-location
    "M-g" #'corfu-info-location ; default
    ;; "M-h" #'corfu-info-documentation    ; default
    "M-h" nil))
(after! meow
  (add-hook 'meow-insert-exit-hook #'corfu-quit))
(setopt corfu-echo-delay '(0.5 . 0.25))

(after! corfu
  (corfu-echo-mode))
(setopt corfu-popupinfo-delay '(1.0 . 0.5))

(after! corfu
  (corfu-popupinfo-mode))
(after! corfu
  (corfu-history-mode))

;; Persist Corfu history across sessions.
(after! (savehist corfu-history)
  (add-to-list 'savehist-additional-variables 'corfu-history))
(package! corfu-terminal
  (after! (corfu)
    (unless (display-graphic-p)
      (corfu-terminal-mode 1))))
(package! kind-icon
  (setopt kind-icon-use-icons (display-graphic-p))
  (setopt kind-icon-blend-background t)

  (require 'kind-icon)

  (after! corfu
    (setopt kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))
(after! kind-icon
  (plist-put kind-icon-default-style :height 0.9))
(after! kind-icon
  ;; <https://github.com/jdtsmith/kind-icon/issues/34#issuecomment-1668560185>
  (add-hook 'ceamx-after-enable-theme-hook #'kind-icon-reset-cache))
(setopt dabbrev-upcase-means-case-search t)
;; FIXME: the `xr'-generated pattern below is mangled from:
;; (setopt dabbrev-ignored-buffer-regexps
;;         '("\\` "
;;           "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?"))

(setopt dabbrev-ignored-buffer-regexps
        (list
         ;; TODO: what does this pattern represent?
         ;;       why is it not same as eval result: (rx "` ")
         "\\` "
         (rx line-start " ")
         (rx (group (or (seq (opt (or "e" (seq "g" (opt "r")))) "tags")
                        "gpath"))
             (optional (group "<" (+ (any numeric)) ">")))))

(after! dabbrev
  (dolist (mode '(doc-view-mode pdf-view-mode tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))
(setopt dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p)
(after! dabbrev
  ;; Swap M-/ and C-M-/
  (keymap-global-set "M-/" #'dabbrev-completion)
  (keymap-global-set "C-M-/" #'dabbrev-expand))
;; FIXME: something is real messed up in here... i think probably these get
;; applied by a mode hook but then never removed...?

(package! cape
 (setopt cape-dabbrev-check-other-buffers t)

  (def-hook! +corfu-add-cape-file-h ()
    'prog-mode-hook
    "Register the `cape-file' `completion-at-point-functions'."
    (add-hook 'completion-at-point-functions #'cape-file 0 t))

  (def-hook! +corfu-add-cape-elisp-block-h ()
    '(org-mode-hook markdown-mode-hook)
    "Register Elisp src block symbol completions."
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  ;; FIXME: weighted too high!
  (def-hook! +corfu-add-cape-dabbrev-h ()
    '(prog-mode-hook
      text-mode-hook
      conf-mode-hook
      comint-mode-hook
      minibuffer-setup-hook
      eshell-mode-hook)
    "Register `dabbrev' completions."
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))

  (def-hook! +corfu-add-cape-elisp-symbol-h ()
    '(emacs-lisp-mode-hook)
    "Register Emacs Lisp symbol completions."
    (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

  ;; (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; FIXME: Enabling any of these outright will override everything else above!
  ;; maybe change order? i.e. move these closer to beginning of expression
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)

  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; "Character Mnemonics & Character Sets": <https://datatracker.ietf.org/doc/html/rfc1345>
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)

  (define-keymap :keymap (current-global-map)
    "M-p p" #'completion-at-point ;; capf
    "M-p t" #'complete-tag        ;; etags
    "M-p d" #'cape-dabbrev        ;; or dabbrev-completion
    "M-p h" #'cape-history
    "M-p f" #'cape-file
    "M-p k" #'cape-keyword
    "M-p s" #'cape-elisp-symbol
    "M-p e" #'cape-elisp-block
    "M-p a" #'cape-abbrev
    "M-p l" #'cape-line
    "M-p w" #'cape-dict
    "M-p :" #'cape-emoji
    "M-p &" #'cape-sgml
    ;; ref: <https://datatracker.ietf.org/doc/html/rfc1345>
    "M-p r" #'cape-rfc1345))
(after! comint
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive))

(after! eglot
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

(after! lsp-mode
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

(provide 'init-completion)
;;; init-completion.el ends here
