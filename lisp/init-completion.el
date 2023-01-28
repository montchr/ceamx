;;; init-completion.el --- Completion interfaces -*- lexical-binding: t; -*-

;; Copyright (c) 2022-2023  Chris Montgomery <chris@cdom.io>

;; Author: Chris Montgomery <chris@cdom.io>
;; URL: https://git.sr.ht/~montchr/ceamx
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;;  Configuration for selection systems.


;;; Sources:

;;  <https://github.com/d12frosted/environment/blob/master/emacs/lisp/init-selection.el>

;;; Code:

;;
;;; === VERTICO :: "VERTical Interactive COmpletion" ================================================
;;  <https://github.com/minad/vertico>

(elpaca-use-package (vertico :host github :repo "minad/vertico" :files (:defaults "extensions/*"))
  :demand t

  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  :config
  ;; Prefix current candidate with arrow
  ;; <https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow>
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
      (setq cand (funcall orig cand prefix suffix index _start))
      (concat
       (if (= vertico--index index)
           (propertize "» " 'face 'vertico-current)
         "  ")
       cand))))

;; Configure directory extension.
(use-feature vertico-directory
  :after vertico
  :commands (vertico-directory-tidy)

  :init
  ;; TODO: what for, exactly? needs binding?
  ;; via <https://github.com/minad/vertico/wiki#additions-for-moving-up-and-down-directories-in-find-file>
  ;; (defun cmx/vertico-directory-delete-entry ()
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

  :general
  ( :keymaps 'vertico-map
    "RET"     #'vertico-directory-enter
    "DEL"     #'vertico-directory-delete-char
    "M-DEL"   #'vertico-directory-delete-word)

  :config
  ;; Tidy shadowed file names -- e.g. cleans `~/foo/bar///' to `/', and `~/foo/bar/~/' to `~/'.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


;;
;;; --- vertico: history ---

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-feature savehist :init (savehist-mode))

(use-feature vertico-repeat
  :after (savehist vertico)
  :commands '(vertico-repeat-history vertico-repeat-save)
  :hook '((minibuffer-setup . #'vertico-repeat-save))
  :config
  (add-to-list 'savehist-additional-variables #'vertico-repeat-history))

(use-feature emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun +vertico/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'+vertico/crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable recursive minibuffers.
  (setq enable-recursive-minibuffers t))


;;
;;; === ORDERLESS ===================================================================================
;;  <https://github.com/oantolin/orderless>
;;  > Emacs completion style that matches multiple regexps in any order 

;; (elpaca-use-package orderless
;;   :init

;;   ;; TODO: Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-space)

;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

(elpaca-use-package orderless
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((equal "!" word) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 1))
        (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles
     '(orderless-initialism
       orderless-literal
       orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))


;;
;;; === MARGINALIA ==================================================================================
;;  <https://github.com/minad/marginalia>
;;  Enable rich completion annotations in the minibuffer.

(elpaca-use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))

  :init
  ;; <https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:37ACBBF7-989F-4A57-9454-06B79B8EB4F0>
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  (with-eval-after-load 'projectile
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file))))


;;
;;; === EMBARK :: Emacs Mini-Buffer Actions Rooted in Keymaps =======================================
;;  <https://github.com/oantolin/embark>

(autoload '+vertico/embark-export-write "lib-vertico" "Export RESULTS to writable buffer")

(elpaca-use-package embark
  :general
  ("C-;" #'embark-act)
  ("M-." #'embark-dwim)
  ( :keymaps 'minibuffer-local-map
    "C-c C-e" '(+vertico/embark-export-write :which-key "Export to writable buffer"))
  (+general-global-help
    "b" '(embark-bindings :which-key "bindings"))

  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;;
;;; === CONSULT :: "Consulting completing-read" =====================================================
;;  <https://github.com/minad/consult>
;;  - <https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html>
;;
;;; Notes:
;;  - `fd' is intentionally excluded from `consult' core.
;;    however, support can be added manually: <https://github.com/minad/consult/wiki#find-files-using-fd>

(elpaca-use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)

  :general
  ([remap bookmark-jump]                   #'consult-bookmark
   [remap repeat-complex-command]          #'consult-complex-command
   [remap evil-show-marks]                 #'consult-mark
   ;; [remap evil-show-jumps]              #'+vertico/jump-list
   [remap goto-line]                       #'consult-goto-line
   [remap imenu]                           #'consult-imenu
   [remap isearch-edit-string]             #'consult-isearch-history
   [remap locate]                          #'consult-locate
   [remap load-theme]                      #'consult-theme
   [remap man]                             #'consult-man
   [remap recentf-open-files]              #'consult-recent-file
   [remap switch-to-buffer]                #'consult-buffer
   [remap switch-to-buffer-other-window]   #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]    #'consult-buffer-other-frame
   [remap yank-pop]                        #'consult-yank-pop
   [remap project-switch-to-buffer]        #'consult-project-buffer)

  ("M-y"  #'consult-yank-pop)                ;; orig. yank-pop

  (;; C-c bindings (mode-specific-map)
   "C-c h"  #'consult-history
   "C-c m"  #'consult-mode-command
   "C-c k"  #'consult-kmacro)

  (;; Custom M-# bindings for fast register access
   "M-#"    #'consult-register-load
   "M-'"    #'consult-register-store          ;; orig. abbrev-prefix-mark (unrelated)
   "C-M-#"  #'consult-register)

  (;; M-g bindings (goto-map)
   "M-g e"  #'consult-compile-error
   "M-g f"  #'consult-flymake               ;; Alternative: consult-flycheck
   "M-g o"  #'consult-outline               ;; Alternative: consult-org-heading
   "M-g m"  #'consult-mark
   "M-g k"  #'consult-global-mark
   "M-g i"  #'consult-imenu
   "M-g I"  #'consult-imenu-multi)

  (;; M-s bindings (search-map)
   "M-s d"  #'consult-find
   "M-s D"  #'consult-locate
   "M-s g"  #'consult-ripgrep
   "M-s G"  #'consult-git-grep
   "M-s l"  #'consult-line
   "M-s L"  #'consult-line-multi
   "M-s k"  #'consult-keep-lines
   "M-s u"  #'consult-focus-lines)

  ;;; Isearch integration
  ("M-s e"   #'consult-isearch-history)
  ( :keymaps 'minibuffer-local-map
    "M-s"    #'consult-history                 ;; orig. next-matching-history-element
    "M-r"    #'consult-history)                ;; orig. previous-matching-history-element
  ( :keymaps 'isearch-mode-map
    "M-s l"  #'consult-line ;; Needed by consult-line to detect isearch.
    "M-s L"  #'consult-line-multi)  ;; Needed by consult-line to detect isearch.

  (+general-global-search
    "s"  'consult-line
    "i"  '(consult-isearch-history :which-key "isearch")
    "o"  '(consult-outline :which-key "outline")
    "p"  '(consult-ripgrep :which-key "project"))

  :config
  (setq consult-preview-key 'any)

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; TODO: Make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;;; Use Orderless as pattern compiler for consult-grep/ripgrep/find.
  ;;; via <https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind>
  ;;; FIXME: both options break!

  ;; (defun +consult--orderless-regexp-compiler (input type &rest _config)
  ;;   (setq input (orderless-pattern-compiler input))
  ;;   (cons
  ;;    (mapcar (lambda (r) (consult--convert-regexp r type)) input)
  ;;    (lambda (str) (orderless--highlight input str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  ;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

  ;; OPTION 2: Activate only for some commands, e.g., consult-ripgrep!
  ;; (defun +consult--with-orderless (&rest args)
  ;;   (minibuffer-with-setup-hook
  ;;       (lambda ()
  ;;         (setq-local consult--regexp-compiler #'+consult--orderless-regexp-compiler))
  ;;     (apply args)))
  ;; (advice-add #'consult-ripgrep :around #'+consult--with-orderless)

  ;;; --- buffers ---

  ;;; Pre-select nearest heading for `consult-org-heading' and `consult-outline'
  ;;; <https://github.com/minad/consult/wiki#pre-select-nearest-heading-for-consult-org-heading-and-consult-outline-using-vertico>

  (defvar +consult--previous-point nil
    "Location of point before entering minibuffer.
  Used to preselect nearest headings and imenu items.")

  (defun +consult--set-previous-point ()
    "Save location of point. Used before entering the minibuffer."
    (setq +consult--previous-point (point)))

  (defun +consult-vertico--update-choose (&rest _)
    "Pick the nearest candidate rather than the first after updating candidates."
    (when (and +consult--previous-point
               (memq current-minibuffer-command
                     '(consult-org-heading consult-outline)))
      (setq vertico--index
            (max 0 ; if none above, choose the first below
                 (1- (or (seq-position
                          vertico--candidates
                          +consult--previous-point
                          (lambda (cand point-pos) ; counts on candidate list being sorted
                            (> (cl-case current-minibuffer-command
                                 (consult-outline
                                  (car (consult--get-location cand)))
                                 (consult-org-heading
                                  (get-text-property 0 'consult--candidate cand)))
                               point-pos)))
                         (length vertico--candidates))))))
    (setq +consult--previous-point nil))

  (advice-add #'consult-org-heading :before #'+consult--set-previous-point)
  (advice-add #'consult-outline :before #'+consult--set-previous-point)
  (advice-add #'vertico--update :after #'+consult-vertico--update-choose)

  ;;; --- project-awareness ---

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;;
;;; === CORFU :: "Completion Overlay Region FUnction" ===============================================
;;  <https://github.com/minad/corfu>

;; Recipe derived from <https://github.com/progfolio/.emacs.d/blob/7935d69dc2bb05af99b3743b9d33826043bfedd8/init.org#corfu>
(elpaca-use-package (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :defer 5
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode))

  :config
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point)))))

(use-feature emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; <https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html>
(use-feature dabbrev
  :bind
  ;; Swap M-/ and C-M-/
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))

  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;;
;;; === CAPE :: "Completion At Point Extensions" ====================================================
;;  <https://github.com/minad/cape>

(elpaca-use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))

  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


;;
;;; === INTEGRATIONS ================================================================================


;;
;;; embark-consult
;;  <https://github.com/minad/consult#embark-integration>

(elpaca-use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;
;;; aff.el :: "Asynchronous Fuzzy Finder for Emacs"
;;  <https://github.com/minad/affe>

(elpaca-use-package affe
  :after (orderless)
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))

  ;; Configure `orderless' as `affe-regexp-compiler' in Consult.
  (defun +affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'+affe-orderless-regexp-compiler))


;;
;;; wgrep :: "Writable grep buffer and apply the changes to files"
;;  <https://github.com/mhayashi1120/Emacs-wgrep>

(elpaca-use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)

  :bind ( :map dired-mode-map
          ("C-c C-e" . wgrep-change-to-wgrep-mode)
          :map grep-mode-map
          ("W" . wgrep-change-to-wgrep-mode))

  :config
  ;; (advice-add #'wgrep-abort-changes :after #'+popup-close-a)
  ;; (advice-add #'wgrep-finish-edit :after #'+popup-close-a)
  )


(provide 'init-completion)
;;; init-completion.el ends here
