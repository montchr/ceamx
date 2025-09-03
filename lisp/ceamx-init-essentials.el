;; -*- lexical-binding: t; -*-

;; Baseline configurations
;; :PROPERTIES:
;; :END:


;; Increase number of messages saved in log.
(setopt message-log-max 10000)

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

(setopt kill-whole-line t)

;; =ceamx-simple=: Simple & common commands


(use-feature! ceamx-simple
  :demand t)

;; =crux= :: a [c]ollection of [r]idiculously [u]seful e[x]tensions


(package! crux
  (after! pulsar
    (cl-pushnew #'crux-other-window-or-switch-buffer pulsar-pulse-functions)))

;; =tmr= :: set timers using a convenient notation

;; + Website :: <https://protesilaos.com/emacs/tmr>


(package! tmr
  (require 'tmr)

  ;; FIXME: conflicts with `ceamx-toggle-prefix'
  ;; (keymap-global-set "C-c T" #'tmr-prefix-map)

  (setopt tmr-notification-urgency 'normal)
  (setopt tmr-description-list 'tmr-description-history)

  (defvar-keymap ceamx+embark-tmr-action-map
    :doc "Action map for TMRs, which can be utilized by Embark."
    "k" #'tmr-remove
    "r" #'tmr-remove
    "R" #'tmr-remove-finished
    "c" #'tmr-clone
    "a" #'tmr-toggle-acknowledge
    "e" #'tmr-edit-description
    "s" #'tmr-reschedule)

  (defvar embark-keymap-alist)
  (defvar embark-post-action-hooks)

  (after! embark
    (add-to-list 'embark-keymap-alist '(tmr-timer . ceamx+embark-tmr-action-map))
    (cl-loop
     for cmd the key-bindings of ceamx+embark-tmr-action-map
     if (commandp cmd) do
     (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart)))))

;; Configure sane window-scrolling behavior


;; Available cycle positions for `recenter-top-bottom'.
(setq! recenter-positions '(top middle bottom))
;; (setq! recenter-positions '(middle top bottom))
                                        ; default

;; Horizontally-scroll only the current line when point column moves
;; beyond window boundaries.
(setq! auto-hscroll-mode 'current-line)

(setq! scroll-error-top-bottom t
       ;; Prevent unwanted horizontal scrolling upon navigation.
       scroll-preserve-screen-position t
       scroll-conservatively 10000)

;; Add a margin when scrolling vertically (or don't).
(setq-default scroll-margin 1)

;; Auto-revert buffers


(progn
  (global-auto-revert-mode 1)

  ;; Ensure the non-file-visiting buffers are also auto-reverted as
  ;; needed.  For example, this will cause Dired to refresh a file list
  ;; when the directory contents have changed (but see also
  ;; `dired-auto-revert-buffer').
  (setopt global-auto-revert-non-file-buffers t)

  (setopt auto-revert-interval 2))

;; Normalize whitespace/spacing/indentation handling
;; :PROPERTIES:
;; :ID:       79ef85d9-c7b3-4860-a516-95145c4825b1
;; :END:


(use-feature! emacs
  :hook ((before-save . delete-trailing-whitespace))

  :config
  (setq-default indent-tabs-mode nil
                tab-width 8)

  (setopt backward-delete-char-untabify-method 'hungry)
  (setopt cycle-spacing-actions '(delete-all-space just-one-space restore))
  (setopt mode-require-final-newline 'visit-save)
  (setopt sentence-end-double-space t)

  (electric-indent-mode 1))

;; Visualize notable and unusual whitespace


(use-feature! emacs
  :hook ((prog-mode . whitespace-mode))

  :config
  (setq-default indicate-empty-lines nil)

  (setopt whitespace-style
          '(face
            tabs
            tab-mark
            trailing
            missing-newline-at-eof
            ;; space-after-tab::space
            ;; space-before-tab::space
            space-after-tab
            space-before-tab)))

;; =editorconfig= :: enforce EditorConfig settings

;; - Website :: <https://editorconfig.org>


;; via <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>
(defun +editorconfig-enforce-org-mode-tab-width-h (props)
  "Prevent `editorconfig' from changing `tab-width' in `org-mode'.
A \"tab-width\" of any value other than 8 is an error state in
org-mode, so it must not be changed.

PROPS is as in `editorconfig-after-apply-functions'."
  (when (and (gethash 'indent_size props)
             (derived-mode-p 'org-mode))
    (setq tab-width 8)))

(package! editorconfig
  (add-hook 'ceamx-emacs-startup-hook #'editorconfig-mode)

  ;; TODO: needs prefix to be defined early!
  ;;  (keymap-global-set "C-c l f e" #'editorconfig-format-buffer)

  (after! editorconfig
    (add-hook 'editorconfig-after-apply-functions
              #'+editorconfig-enforce-org-mode-tab-width-h)))

;; Integrate with system clipboard


(setopt save-interprogram-paste-before-kill t)

;; =mwim=: Replace ~beginning-of-line~ and ~end-of-line~ with DWIM alternatives


(package! mwim)

;; =beginend= :: rebind context-sensitive =(beginning,end)-of-buffer=

;; + Package :: https://github.com/DamienCassou/beginend


(package! beginend
  (beginend-global-mode))

;; =easy-kill= :: killing is easy when you're emacs :package:

;; + Package documentation :: <https://github.com/leoliu/easy-kill/blob/master/README.rst>

;; + =w= :: word
;; + =s= :: sexp
;; + =l= :: list
;; + =d= :: defun
;; + =D= :: defun name
;; + =f= :: file
;; + =b= :: buffer name
;;   + =-= :: ~default-directory~
;;   + =+= :: absolute path
;;   + =0= :: basename


(use-package easy-kill
  :ensure t)

;; Replace region when inserting text


(delete-selection-mode 1)

;; =expreg= :: simple alternativate to ~expand-region~ using ~treesit~ :package:

;; + Package :: <https://github.com/casouri/expreg>

;; I’ve become a big fan of this package recently, even though it’s just
;; been sitting here for a while.


(package! expreg)

;; =drag-stuff= :: drag stuff around in arbitrary directions :package:

;; <https://github.com/rejeep/drag-stuff.el>

;; This package appears to be abandoned since 2017.  As of <2025-06-28>,
;; it still works relatively well, but has some issues:

;; + Does not work in some Tree-Sitter modes (e.g. JavaScript/TypeScript/JSON)
;; + Possible subtle conflicts with ~org-metaup~ and ~org-metadown~?
;; + Numerous warnings about deprecated functions <https://github.com/rejeep/drag-stuff.el/issues/36>

;; I haven't yet found any other package to move arbitrary regions
;; up/down while preserving column position.

;; ~move-text-mode~ <https://github.com/emacsfodder/move-text> claims to do
;; this, but fails pretty badly, moving the region/selection to the first
;; column regardless of its original position.


(use-package drag-stuff
  :demand t)

;; Visualize and electrify matching character pairs :pairs:

;; See the Info node [[info:emacs#Matching]]


(setopt blink-matching-paren t)
;; Avoid "expression" style, which looks too much like a selected region.
(setopt show-paren-style 'parenthesis)

(setopt electric-pair-preserve-balance t)
(setopt electric-pair-delete-adjacent-pairs t)
(setopt electric-pair-skip-whitespace t)
;; TODO: evaluating...
(setopt electric-pair-open-newline-between-pairs t)

(electric-pair-mode 1)
(show-paren-mode 1)

;; Consider camelCaseWORDs as separate words
;; :PROPERTIES:
;; :END:

;; While it can be useful in some contexts, I wish that ~subword-mode~ did not break
;; ~ceamx/backward-kill-word~.  See also [[*Provide a command to intelligently kill
;; words backwardsly]]


(global-subword-mode 1)

;; =ialign= :: Interactively ~align-regexp~ :package:

;; <https://github.com/mkcms/interactive-align/blob/master/README.org#usage>


(package! ialign)

;; TODO =string-inflection= :: Cycle through character casing :package:cycling:

;; Needs better bindings.


(package! string-inflection)

(use-feature! ceamx-editor
  :demand t
  :after string-inflection)

;; =typo= :: Cycle typographical characters :package:cycling:
;; :LOGBOOK:
;; - Refiled on [2025-08-08 Fri 09:45]
;; :END:

;; + Package :: <https://github.com/jorgenschaefer/typoel>

;; *Before*:	The "quick" brown --- ffiox.
;; *After*:	The “quick” brown — ffiox.


(require 'ceamx-lang)

(package! (typo :host github :repo "jorgenschaefer/typoel")
  ;; Provides [C-c 8] prefix for Unicode entry to complement [C-x 8].
  ;; FIXME: there must be another way
  ;; (typo-global-mode 1)

  (def-hook! ceamx-lang-typo-mode-maybe-enable-h ()
    'text-mode-hook
    "Conditionally enable `typo-mode'.
`typo-mode' will not be enabled when the current major-mode is one of
the major-modes listed in the `ceamx-lang-typo-mode-excluded-modes' user
setting or the `ceamx-text-mode-derived-prog-modes-list' constant, which
see."
    (let ((excluded-modes (append ceamx-text-mode-derived-prog-modes-list
                                  ceamx-lang-typo-mode-excluded-modes)))
      (unless (derived-mode-p excluded-modes)
        (typo-mode 1)))))

;; =cycle-at-point= :: Promptless text cycling at point :package:cycling:
;; :LOGBOOK:
;; - Refiled on [2025-08-08 Fri 09:45]
;; :END:


(package! cycle-at-point)

;; =shift-number= :: Increase/decrease the number at point :package:cycling:
;; :LOGBOOK:
;; - Refiled on [2025-08-08 Fri 09:45]
;; :END:

;; + Package :: <https://codeberg.org/ideasman42/emacs-shift-number>

;; Provides commands ~shift-number-up~ and ~shift-number-down~ to increment or
;; decrement the number at point, respectively.


;; TODO: add to embark actions
(package! shift-number)

;; TODO =cycle-quotes= :: Cycle quote characters surrounding string at point :package:cycling:


(package! cycle-quotes)

;; Modal editing


(package! (bray :host codeberg :repo "ideasman42/emacs-bray")
  (require 'bray)
  (require 'ceamx-modaled))

(use-feature! ceamx-modaled
  :demand t
  :after (bray)
  ;; :hook ((ceamx-after-init . ceamx-modaled-mode))
  :defines ( ceamx-modaled-state-normal-map
             ceamx-modaled-state-insert-map)
  :config
  (setopt bray-state-default 'normal)
  (setopt bray-state-definitions
          '(( :id normal
              :cursor-type hollow
              :lighter "Ⓝ"
              :keymaps ((t . ceamx-modaled-state-normal-map)))
            ( :id insert
              :cursor-type bar
              :lighter "Ⓘ"
              :keymaps ((t . ceamx-modaled-state-insert-map))
              :is-input t))))

;; Configure line wrapping aka “fill” and ~auto-fill-mode~

;; By default, automatically hard-wrap text in ~prog-mode~ and ~text-mode~:


(use-feature! emacs
  :hook (((prog-mode text-mode) . auto-fill-mode)))



;; The original ~fill-column~ value of "70" behaves more-ideally than the
;; expected "80".  While 80 may be some sort of standard for code, 70
;; works better for prose.  70 helps avoid the end of lines getting
;; visually cut off by giving some space for the frame's interface
;; elements (e.g. spacing, line numbers, gutter indicators, etc.).  72
;; feels like a good compromise?


(use-feature! emacs
  :config
  (setq-default fill-column 72))



;; That said, unfortunately, this value applies to /all/ text, not just
;; prose.  While ~comment-auto-fill-only-comments~ exists, it does not really
;; work so well in prose-orientated text documents, as even these may
;; define a comment syntax (e.g. =text/markdown= and the not-yet-standard
;; =text/org=).  For that reason, I enable it locally on ~prog-mode-hook~.


(def-hook! ceamx-prog-mode-auto-fill-comments-only-h ()
  '(prog-mode-hook)
  "Enable `comment-auto-fill-only-comments' setting in `prog-mode'.
Common text modes including `markdown-mode' and `org-mode' also
  define their own comment syntax, resulting in no auto-fill anywhere
  except comments, which is not what `text-mode' is about."
  (setq-local comment-auto-fill-only-comments t))



;; I am not interest in soft wrapping, as it tends to lead to confusion
;; about the actual contents of files.  Instead, let the window visually
;; truncate the line of text.

;; Also, related, see ~auto-hscroll-mode~, which can help expose the
;; truncated portion of a line automatically.


(use-feature! emacs
  :config
  (setq-default truncate-lines t))

;; =unfill= :: Reverse/toggle filling/hard-wrapping text :package:keybinds:

;; + Package :: <https://github.com/purcell/unfill>


(use-package unfill
  :demand t)

;; Configure secrets lookup with ~auth-source~ and =password-store=

;; - source :: <https://github.com/jwiegley/dot-emacs/blob/9d595c427136e2709dee33271db1a658493265bd/init.org#auth-source-pass>


(use-feature! auth-source
  :demand t
  :config
  ;; Ensure the usage of an encrypted auth credentials file.  It's
  ;; best to list only a single file here to avoid confusion about
  ;; where secrets might be stored.
  (setopt auth-sources (list "~/.authinfo.gpg")))

;; TODO: provide explanation as to why these functions are named like so -- they just magically work..?
(use-feature! auth-source-pass
  :demand t

  :preface
  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

  (defun auth-source-pass--reset-cache ()
    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

  (defun auth-source-pass--read-entry (entry)
    "Return a string with the file content of ENTRY."
    (run-at-time 45 nil #'auth-source-pass--reset-cache)
    (let ((cached (gethash entry auth-source-pass--cache)))
      (or cached
          (puthash
           entry
           (with-temp-buffer
             (insert-file-contents (expand-file-name
                                    (format "%s.gpg" entry)
                                    (getenv "PASSWORD_STORE_DIR")))
             (buffer-substring-no-properties (point-min) (point-max)))
           auth-source-pass--cache))))

  (defun ceamx-auth-source-pass-list-items ()
    "Return a list of all password store items."
    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
      (mapcar
       (lambda (file)
         (file-name-sans-extension (file-relative-name file store-dir)))
       (directory-files-recursively store-dir "\.gpg$"))))

  :config
  (auth-source-pass-enable))



;; Add function to simply get an auth entry from the password store:


(defun ceamx-auth-source/lookup (host user &optional port)
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

;; Configure GnuPG integration with ~epa~ & ~epg~


(use-feature! epa
  :demand t
  :config
  (require 'ceamx-cryption)

  ;; HACK: No need to set a recipient every single time!
  (setq-default epa-file-encrypt-to (ceamx-cryption+epa-default-recipient))
  (advice-add #'epa-file-write-region :before #'ceamx-cryption+epa-disable-key-prompt-a)

  ;; Enable automatic cryption of *.gpg files.
  (epa-file-enable))

(use-feature! dired
  :after epa
  :config
  ;; Also see the ':' prefix!
  (define-keymap :keymap dired-mode-map
    "C-c k e" #'epa-dired-do-encrypt
    "C-c k d" #'epa-dired-do-decrypt))

(use-feature! epg
  :config
  ;; Handle pinentry within Emacs.
  (setopt epg-pinentry-mode 'loopback))

;; Buttonize URLs and email addresses with ~goto-address~ [builtin]


(use-feature! goto-addr
  :demand t
  :hook (prog-mode . goto-address-prog-mode))

;; =link-hint=: Activate links in buffer with ~avy~

;; <https://github.com/noctuid/link-hint.el>


(package! link-hint)

;; Manage backup files and prevent file-lock clutter


(use-feature! emacs
  :config
  (setopt create-lockfiles nil
          ;; TODO: enable under some conditions e.g. not a project,
          ;; tramp remote file
          make-backup-files nil
          delete-by-moving-to-trash t)

  (when make-backup-files
    (setopt version-control t
            delete-old-versions t
            kept-new-versions 5
            kept-old-versions 5)))

;; Configure finding of files
;; :PROPERTIES:
;; :ID:       d7168d7e-0419-4d6c-b29e-002c826be6c3
;; :END:


(use-feature! emacs
  :config
  (setopt find-file-suppress-same-file-warnings t
          find-file-visit-truename t)

  ;; via <https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/lisp/doom-editor.el#L78-L89>
  (def-hook! ceamx-find-file-create-paths-h ()
    'find-file-not-found-functions
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; Auto-save file-visiting buffers :buffer:


(use-feature! emacs
  :config
  (setopt
   ;; Prevent creation of the list of all auto-saved files.
   auto-save-list-file-prefix nil
   ;; Number of input events before autosave
   auto-save-interval 300
   ;; Idle interval for all file-visiting buffers
   auto-save-visited-interval 30
   ;; Idle interval before autosave
   auto-save-timeout 30
   ;; Don't create auto-save "~" files.
   auto-save-default nil)

  ;; Save file-visiting buffers according to the configured timers.
  (auto-save-visited-mode))

;; Interactive buffer management with ~ibuffer~ :buffers:


(use-feature! emacs
  :config
  (setopt ibuffer-movement-cycle t))

;; =casual-suite=: transient-dispatch menus for complex modes


(package! casual-suite
  (require 'casual-suite)

  (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
  (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)

  ;; <https://github.com/kickingvegas/casual-avy>
  ;; M-g M-g
  (keymap-set goto-map "M-g" #'casual-avy-tmenu)

  ;; <https://github.com/kickingvegas/casual-calc>
  (after! calc
    (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))
  (after! calc-alg
    (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu))

  ;; <https://github.com/kickingvegas/casual-dired>
  (after! dired
    (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu))

  ;; <https://github.com/kickingvegas/casual-info>
  (after! info
    (keymap-set Info-mode-map "C-o" #'casual-info-tmenu))

  ;; <https://github.com/kickingvegas/casual-isearch>
  (after! isearch
    (keymap-set isearch-mode-map "<f2>" #'casual-isearch-tmenu))

  (after! ibuffer
    (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
    (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
    (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu))

  (after! re-builder
    (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
    (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu))

  (after! bookmark
    (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu))

  (after! org-agenda
    (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)))

;; =Info-mode= enchantments


(use-feature! info
  :hook ((Info-mode . hl-line-mode)
         (Info-mode . scroll-lock-mode)))

;; =helpful=: Provide improved alternatives to the builtin "describe" utilities

;; - Source code :: <https://github.com/Wilfred/helpful>

;; Note that there is a severe but edge-case bug that has gone unfixed
;; for quite a while.  ~helpful~ cannot display documentation for symbols
;; defined in Emacs C source code:

;; <https://github.com/Wilfred/helpful/issues/329>


(package! helpful
  (defer! 1
    (require 'helpful)))

;; Record some variables' values with ~savehist~


(use-feature! savehist
  :demand t
  :init
  (savehist-mode)

  :config
  (cl-dolist (save '(kill-ring
                     regexp-search-ring
                     search-ring))
    (cl-pushnew save savehist-additional-variables))
  (setopt history-length 333
          history-delete-duplicates t)
  (setopt savehist-autosave-interval 60))

;; Record point position in buffers with ~saveplace~


(use-feature! saveplace
  :demand t
  :init
  (save-place-mode))

;; Record recently-accessed files with ~recentf~


(use-feature! recentf
  :demand t
  :init
  (recentf-mode)

  :config
  (setopt recentf-max-saved-items 50)   ; default => 20
  (setopt recentf-max-menu-items 15)    ; default => 10

  ;; Disable recentf-cleanup on Emacs start, because it can cause
  ;; problems with remote files.
  (setopt recentf-auto-cleanup 'never)

  ;; Exclude internal plumbing files.
  (dolist (path '(ceamx-etc-dir ceamx-var-dir))
    (add-to-list 'recentf-exclude path)))

;; Increase undo history limits

;; Advice from the author of ~undo-fu~:

;; #+begin_quote
;; The default undo limits for emacs are quite low _(0.15mb at time of
;; writing)_ undo-tree for example increases these limits.

;; On modern systems you may wish to use much higher limits.

;; This example sets the limit to 64mb, 1.5x (96mb) for the strong limit
;; and 10x (960mb) for the outer limit.  Emacs uses 100x for the outer
;; limit but this may be too high when using increased limits.
;; #+end_quote

;; via <https://codeberg.org/ideasman42/emacs-undo-fu#undo-limits>


(setopt undo-limit 67108864) ; 64mb.
(setopt undo-strong-limit 100663296) ; 96mb.
(setopt undo-outer-limit 1006632960) ; 960mb.

;; =undo-fu-session=: Record undo/redo steps across Emacs sessions

;; - Source code :: <https://codeberg.org/ideasman42/emacs-undo-fu-session>

;; NOTE: This is *NOT* just for use with ~undo-fu~!  It's an essential
;; enhancement to the builtin Emacs undo system as well.


(defvar undo-fu-session-directory
  (expand-file-name "undo-fu-session" ceamx-var-dir))

(package! undo-fu-session
  (setopt undo-fu-session-incompatible-files '("/git-rebase-todo\\'"))
  (setopt undo-fu-session-ignore-temp-files t)
  (setopt undo-fu-session-ignore-encrypted-files t)

  (setopt undo-fu-session-compression 'zst)

  (undo-fu-session-global-mode))

;; =vundo=: Visualize the Emacs undo tree

;; - Source code :: <https://github.com/casouri/vundo>


(use-package vundo
  :defer t
  :defines vundo-unicode-symbols
  :config
  (setq! vundo-glyph-alist vundo-unicode-symbols))

;; =bookmark-in-project= :: Functions to limit bookmark queries to current project
;; :PROPERTIES:
;; :END:


(package! (bookmark-in-project
           :host codeberg
           :repo "ideasman42/emacs-bookmark-in-project"))

;; macOS: Remap modifier keys for the Apple keyboard layout :keybinds:


(when (and (ceamx-host-macos-p) (display-graphic-p))
  (setopt mac-control-modifier 'control)
  (setopt mac-option-modifier 'meta)
  (setopt ns-option-modifier 'meta)
  (setopt mac-command-modifier 'super)
  (setopt ns-command-modifier 'super)
  ;; Free up the right-side option key for character composition.
  (setopt mac-right-option-modifier 'none)
  (setopt ns-right-option-modifier 'none)
  ;; Common system hotkeys.
  (define-keymap :keymap (current-global-map)
    "s-c" #'kill-ring-save
    "s-v" #'yank
    "s-x" #'kill-region
    "s-q" #'save-buffers-kill-emacs))

;; Enable and configure ~repeat-mode~ :keybinds:


(repeat-mode 1)

(setopt repeat-exit-timeout 10)
(setopt repeat-on-final-keystroke t)
(setopt repeat-keep-prefix t)
(setopt repeat-exit-key "<return>")



;; Related, but not technically part of ~repeat-mode~:


(setopt set-mark-command-repeat-pop t)

;; Provide ~ceamx-init-essentials~ feature


(provide 'ceamx-init-essentials)
;;; ceamx-init-essentials.el ends here
