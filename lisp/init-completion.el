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
  (setopt vertico-count 8)
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
           (consult-imenu buffer)
           (consult-org-heading ,(lambda (_) (text-scale-set -1)))))

  (setopt vertico-multiform-categories
          '((buffer flat (vertico-cycle . t))
            (consult-grep buffer)
            (imenu (:not indexed mouse))
            (symbol (vertico-sort-function . vertico-sort-alpha))))

  (vertico-multiform-mode))
(package! consult
  (require 'consult))
(setopt consult-narrow-key ">")         ; suggested: "<"
(setopt consult-preview-key 'any)
;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
(with-eval-after-load 'consult
  (dolist (hook '(;; local modes on prog-mode hooks
                  hl-todo-mode
                  elide-head-mode
                  ;; enabled global modes
                  global-org-modern-mode
                  global-hl-todo-mode))
    (add-to-list 'consult-preview-allowed-hooks hook)))
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
(setopt register-preview-delay 0.5)
(setopt register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)
(setopt xref-show-definitions-function #'consult-xref)
(setopt xref-show-xrefs-function #'consult-xref)
(defun +consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred)))
(after! (consult consult-imenu pulsar)
  (setq consult-after-jump-hook nil)
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))
(define-keymap :keymap (current-global-map)
  "C-c M-x" #'consult-mode-command
  "C-c h" #'consult-history
  "C-c k" #'consult-kmacro
  "C-c m" #'consult-man
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
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'embark-prefix-help-command))
(package! marginalia
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle)

  (marginalia-mode))
(after! (marginalia)
  (when (featurep 'projectile)
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file))))
(package! orderless
  (require 'orderless)

  ;; Spaces & dash & slash & underscore
  (setopt orderless-component-separator " +\\|[-/_]"))
(defun +orderless-fast-dispatch (word index total)
  "Fast-dispatch `orderless' completion style for `corfu'."
  (and (= index 0) (= total 1) (length< word 4)
       (cons 'orderless-literal-prefix word)))
(after! orderless
  (orderless-define-completion-style +orderless-fast
    "Fast completion style, intended for usage with `corfu'."
    (orderless-style-dispatchers '(+orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))
(after! orderless
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp))))
(after! orderless
  (setopt completion-styles '(;; +orderless-fast
                              +orderless-with-initialism
                              ;; orderless
                              basic))

  (setopt completion-category-overrides
          '(;; The "file" category override is required for TRAMP hostname
            ;; completion.  "partial-completion" allows for wildcard/partial
            ;; paths (e.g. "/u/s/l" for "/usr/share/local").
            (file (styles basic partial-completion))))

  ;; Available matching styles:
  ;;
  ;; - `orderless-regexp'
  ;; - `orderless-literal'
  ;; - `orderless-literal-prefix'
  ;; - `orderless-prefixes' :: split component at word bounds and match word
  ;;   bounds in candidate (in order)
  ;; - `orderless-initialism' :: each char is the beginning of a word in
  ;;   candidate, in order;  e.g. ‘abc’ => ‘\<.*\<b.*\c’
  ;; - `orderless-flex' :: chars in component appear in candidate in order, but
  ;;   not necessarily consecutively;  e.g. ‘abc’ => ‘a.*b.*c’
  ;;
  ;; <https://github.com/oantolin/orderless/blob/master/README.org#component-matching-styles>
  ;; (setopt orderless-matching-styles (list #'orderless-literal
  ;;                                         #'orderless-regexp))

  ;; Style dispatchers:
  ;;
  ;; `orderless-affix-dispatch' :: support a mini-syntax using special
  ;; characters as prefix or suffix, as follows:
  ;;
  ;; - '!' :: negate component with `orderless-not' e.g. "!bad" and "bad!" match
  ;;          strings that do *not* contain "bad"
  ;; - '&' :: match candidate by annotation with `orderless-annotation'
  ;; - ',' :: use `orderless-initialism'
  ;; - '=' :: use `orderless-literal'
  ;; - '^' :: use `orderless-literal-prefix'
  ;; - '~' :: use `orderless-flex'
  ;; - '%' :: ignore diacritics and character inflections
  ;; (setopt orderless-style-dispatchers (list #'orderless-affix-dispatch))


  )
(package! corfu
  (require 'corfu)

  (setopt corfu-count 12
          corfu-cycle t
          corfu-max-width 80
          corfu-min-width 25
          corfu-scroll-margin 5)
  ;; cf. `orderless-component-separator'
  (setopt corfu-separator ?_)
  (setopt corfu-on-exact-match 'insert
          corfu-preselect 'prompt
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match t)

  (keymap-set corfu-map "M-SPC" #'corfu-insert-separator)

  ;; Prevent excessive completion-spamming.
  ;; Without this, on Emacs 30.0, typing causes constant `corfu' errors.
  ;; <https://github.com/minad/corfu/discussions/457>
  (setopt text-mode-ispell-word-completion nil)

  (global-corfu-mode))
(setopt corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2)
(defun +corfu-minibuffer-enable-p ()
  "Whether to enable `corfu' completion in a currently-active minibuffer."
  (not (or (bound-and-true-p mct--active)
           (bound-and-true-p vertico--input)
           (eq (current-local-map) read-passwd-map))))
(setopt global-corfu-minibuffer #'+corfu-minibuffer-enable-p)
(after! meow
  (add-hook 'meow-insert-exit-hook #'corfu-quit))
(setopt corfu-echo-delay '(0.25 . 0.25))

(after! corfu
  (unless corfu-popupinfo-mode
    (corfu-echo-mode 1)))
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
(setopt dabbrev-ignored-buffer-regexps '("\\` "))

(after! dabbrev
  (dolist (mode '(doc-view-mode pdf-view-mode tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes mode)))
(setopt dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p)
(package! cape
  ;; Ensure keymap availability.
  (require 'cape)

  (setopt cape-dabbrev-check-other-buffers t)

  (def-hook! +corfu-add-cape-file-h ()
    'prog-mode-hook
    "Register the `cape-file' `completion-at-point-functions'."
    (add-hook 'completion-at-point-functions #'cape-file 0 t))

  (def-hook! +corfu-add-cape-elisp-block-h ()
    '(org-mode-hook markdown-mode-hook)
    "Register Elisp src block symbol completions."
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  (def-hook! +corfu-add-cape-dabbrev-h ()
    '(prog-mode-hook
      text-mode-hook
      conf-mode-hook
      comint-mode-hook
      eshell-mode-hook)
    "Register `dabbrev' completions."
    (add-hook 'completion-at-point-functions #'cape-dabbrev 40 t))

  ;; TODO: also for config.org
  (def-hook! +corfu-add-cape-elisp-symbol-h ()
    '(emacs-lisp-mode-hook)
    "Register Emacs Lisp symbol completions."
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol 30 t))

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

  (keymap-global-set "C-c p" cape-prefix-map))
(after! comint
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive))

(after! eglot
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

(after! lsp-mode
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

(provide 'init-completion)
;;; init-completion.el ends here
