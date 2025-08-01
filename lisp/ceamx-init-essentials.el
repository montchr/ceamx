;; -*- lexical-binding: t; -*-

;; Baseline configurations
;; :PROPERTIES:
;; :ID:       9d89d38e-1fa6-41e5-b5a7-8c1c3422a34c
;; :END:


;; Increase number of messages saved in log.
(setopt message-log-max 10000)

;; "A second, case-insensitive pass over `auto-mode-alist' is time wasted."
(setopt auto-mode-case-fold nil)

(setopt kill-whole-line t)

(define-keymap :keymap (current-global-map)
  "M-c" #'capitalize-dwim
  "M-f" #'forward-word
  "M-F" #'forward-symbol
  "M-l" #'downcase-dwim
  "M-o" #'delete-blank-lines
  "M-Q" #'repunctuate-sentences
  "M-u" #'upcase-dwim
  "M-z" #'zap-up-to-char                ; orig: `zap-to-char'
  "M-=" #'count-words
  "M-SPC" #'cycle-spacing

  "C-h F" #'apropos-function
  "C-h L" #'apropos-library
  "C-h U" #'apropos-user-option
  "C-h V" #'apropos-variable

  ;; TODO: move to window config
  "C-x O" #'next-multiframe-window

  ;; Minimizing frames is the job of the window manager.
  "C-x C-z" nil)

;; Keymap for buffers
;; TODO: copy some of these to `ceamx-toggle-prefix'
(define-keymap :keymap ctl-x-x-map
  "f" #'follow-mode
  "l" #'visual-line-mode
  "r" #'rename-uniquely)

;; =ceamx-simple=: Simple & common commands
;; :PROPERTIES:
;; :ID:       3fcbca20-29f3-4e91-ba76-6bad1199adc3
;; :END:


(use-feature! ceamx-simple
  :demand t

  :config
  (define-keymap :keymap ceamx-file-prefix
  ;; TODO
  ;; "y" #'+yank-this-file-name

  "c" '("copy..." . ceamx-simple/copy-current-file)
  ;; FIXME: don’t prevent deletion (or allow override with prefix)
  "d" '("delete" . ceamx-simple/delete-current-file)
  "f" #'find-file
  "F" #'find-file-other-window
  "r" '("move..." . ceamx-simple/move-current-file)
  "s" #'save-buffer
  "U" #'ceamx-simple/sudo-find-file

  "C-d" '("diff with..." . ceamx-simple/diff-with-file))

  (define-keymap :keymap ceamx-insert-prefix-map
    "d" #'ceamx-simple/insert-date)

  (define-keymap :keymap (current-global-map)
    "ESC ESC" #'ceamx/keyboard-quit-dwim
    "C-g" #'ceamx/keyboard-quit-dwim

    "C-x k" #'ceamx-simple/kill-current-buffer ; orig: `kill-buffer'
    "C-x K" #'kill-buffer

    ;; FIXME: move defun to `ceamx-simple'
    "M-DEL" #'ceamx/backward-kill-word

    "C-M-SPC" #'ceamx-simple/mark-sexp

    ;; Commands for lines
    ;; TODO: currently `easy-kill'
    ;; "M-w" #'ceamx-simple/kill-ring-save
    "M-k" #'ceamx-simple/kill-line-backward
    "C-S-d" #'ceamx-simple/duplicate-line-or-region
    ;; TODO: redundant with `easy-kill'
    "C-S-w" #'ceamx-simple/copy-line
    "C-S-y" #'ceamx-simple/yank-replace-line-or-region
    "C-RET" #'ceamx-simple/new-line-below
    "C-S-RET" #'ceamx-simple/new-line-above

    ;; Commands for text insertion or manipulation
    "C-<" #'ceamx-simple/escape-url-dwim
    "M-Z" #'ceamx-simple/zap-to-char-backward

    ;; Commands for buffers
    "M-s b" #'ceamx-simple/buffers-major-mode
    "M-s v" #'ceamx-simple/buffers-vc-root)

  (define-keymap :keymap prog-mode-map
    "M-RET" #'ceamx-simple/continue-comment)

  (keymap-substitute (current-global-map)
                     #'default-indent-new-line
                     #'ceamx-simple/continue-comment))

;; =crux= :: a [c]ollection of [r]idiculously [u]seful e[x]tensions
;; :PROPERTIES:
;; :ID:       5fe13339-e827-421a-a059-f0fea7bff481
;; :END:


(package! crux
  (define-keymap :keymap (current-global-map)
    "C-k" #'crux-smart-kill-line
    "C-^" #'crux-top-join-line

    "C-x 4 t" #'crux-transpose-windows

    "C-S-d" #'crux-duplicate-current-line-or-region
    "C-S-RET" #'crux-smart-open-line-above
    "C-M-S-d" #'crux-duplicate-and-comment-current-line-or-region

    "M-o" #'crux-other-window-or-switch-buffer ; orig. `delete-blank-lines'

    "S-RET" #'crux-smart-open-line)

  (define-keymap :keymap ceamx-buffer-prefix
    "f" #'crux-cleanup-buffer-or-region
    "M-w" #'crux-kill-buffer-truename)

  (define-keymap :keymap ceamx-file-prefix
    "c" #'crux-copy-file-preserve-attributes
    "d" #'crux-delete-file-and-buffer
    "r" #'crux-rename-file-and-buffer)

  (after! pulsar
    (cl-pushnew #'crux-other-window-or-switch-buffer pulsar-pulse-functions)))

;; =tmr= :: set timers using a convenient notation
;; :PROPERTIES:
;; :ID:       67caf305-67ce-42b5-9ff3-98b0f9ac6b06
;; :END:

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

;; =persistent-scratch= :: Preserve the scratch buffer across sessions
;; :PROPERTIES:
;; :ID:       a85e81df-a194-4ecf-b1ca-5d95279cb45f
;; :END:


(package! persistent-scratch
  (persistent-scratch-setup-default))

;; Configure sane window-scrolling behavior
;; :PROPERTIES:
;; :ID:       c6408cc7-1382-4433-bde2-947e4dc2c062
;; :END:


(use-feature! window
  :bind
  ("C-x <" . scroll-right)
  ("C-x >" . scroll-left)
  ("<wheel-left>" . scroll-left)
  ("<wheel-right>" . scroll-right)

  :config
  ;; Available cycle positions for `recenter-top-bottom'.
  ;; (setopt recenter-positions '(top middle bottom))
  (setopt recenter-positions '(middle top bottom)) ; default

  ;; Horizontally-scroll only the current line when point column moves
  ;; beyond window boundaries.
  (setopt auto-hscroll-mode 'current-line)

  (setopt scroll-error-top-bottom t
          ;; Prevent unwanted horizontal scrolling upon navigation.
          scroll-preserve-screen-position t
          scroll-conservatively 10000)

  ;; Add a margin when scrolling vertically (or don't).
  (setq-default scroll-margin 1))

;; Auto-revert buffers


(use-feature! autorevert
  :hook (ceamx-after-init . global-auto-revert-mode)
  :config
  ;; Ensure the non-file-visiting buffers are also auto-reverted as needed.  For
  ;; example, this will cause Dired to refresh a file list when the directory
  ;; contents have changed.
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
;; :PROPERTIES:
;; :ID:       d0133690-e4a8-40a7-abcf-12816589d4b7
;; :END:

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

;; Movement / Regions / Basic Editing
;; :PROPERTIES:
;; :ID:       2c6a6df3-2b31-4638-bdcd-c9e63e367ea0
;; :END:


(define-keymap :keymap (current-global-map)
  ;; Since `comment-dwim' is bound to [M-;], I find it unintuitive
  ;; that `comment-line' is bound to [C-x C-;].
  "C-x M-;" #'comment-line)

;; Integrate with system clipboard
;; :PROPERTIES:
;; :ID:       7ceb88fc-04e5-47af-a978-c2110076d374
;; :END:


(setopt save-interprogram-paste-before-kill t)

;; =mwim=: Replace ~beginning-of-line~ and ~end-of-line~ with DWIM alternatives


(package! mwim
  ;; FIXME: overrides `org-mode' bindings!
  (keymap-global-set "C-a" #'mwim-beginning)
  (keymap-global-set "C-e" #'mwim-end))

;; =beginend= :: rebind context-sensitive =(beginning,end)-of-buffer=
;; :PROPERTIES:
;; :ID:       7529f1a7-461c-4731-a7b8-05f6ce2104fa
;; :END:

;; + Package :: https://github.com/DamienCassou/beginend


(package! beginend
  (beginend-global-mode))

;; =easy-kill= :: killing is easy when you're emacs :package:
;; :PROPERTIES:
;; :ID:       a3727c22-4373-47aa-ac61-e1355c5e048d
;; :END:

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
  :ensure t
  :bind
  ("M-w" . #'easy-kill)   ; orig. `kill-ring-save'
  ("C-M-@" . #'easy-mark) ; orig. `mark-sexp'
  )

;; Replace region when inserting text


(delete-selection-mode 1)

;; =expreg= :: simple alternativate to ~expand-region~ using ~treesit~

;; + Package :: <https://github.com/casouri/expreg>


(package! expreg
  (keymap-global-set "C-+" #'expreg-expand)
  (keymap-global-set "C-=" #'expreg-contract))

;; =drag-stuff= :: drag stuff around in arbitrary directions :package:
;; :PROPERTIES:
;; :ID:       1febb9e9-ec19-4765-b6bb-21613e7667fb
;; :END:

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
  ;; :ensure t
  :bind
  (([M-up] . drag-stuff-up)
   ([M-right] . drag-stuff-right)
   ([M-down] . drag-stuff-down)
   ([M-left] . drag-stuff-left)))

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
;; :ID:       b0593fa1-e228-4fcd-89f2-f7bbdf5f9d2c
;; :END:

;; While it can be useful in some contexts, I wish that ~subword-mode~ did not break
;; ~ceamx/backward-kill-word~.  See also [[*Provide a command to intelligently kill
;; words backwardsly]]


(global-subword-mode 1)

;; TODO ~string-inflection~ :: commands to cycle through word casing
;; :PROPERTIES:
;; :ID:       bf713b67-b25a-41c7-87ba-f5c9a9f7852b
;; :END:

;; Needs better bindings.


(package! string-inflection)

(use-feature! ceamx-editor
  :after string-inflection
  :commands (ceamx/cycle-string-inflection)
  :init
  (defvar-keymap ceamx-string-repeat-map
    :repeat t

    "c" #'ceamx/cycle-string-inflection))

;; =ialign= :: Interactively ~align-regexp~ :package:
;; :PROPERTIES:
;; :ID:       7c7b2b24-f6e4-4af5-934f-0d66b65bba6c
;; :END:

;; <https://github.com/mkcms/interactive-align/blob/master/README.org#usage>


(package! ialign
  (keymap-global-set "C-x l" #'ialign))

;; Modal editing
;; :PROPERTIES:
;; :ID:       1293c641-aecb-43ab-b001-366e7e0543c8
;; :END:


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
;; :PROPERTIES:
;; :ID:       6590d8b1-eea1-4467-8379-89ee33c0841d
;; :END:

;; + Package :: <https://github.com/purcell/unfill>


(use-package unfill
  :bind ("M-q" . unfill-toggle))

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

;; Configure GnuPG integration with ~epa~ & ~epg~
;; :PROPERTIES:
;; :ID:       99415168-3785-4d17-bea0-2c71f0422de1
;; :END:


(use-feature! epa
  :demand t
  :config
  (require 'ceamx-cryption)

  (define-keymap :keymap ceamx-file-prefix
    "e" #'epa-encrypt-file
    "d" #'epa-decrypt-file)

  (define-keymap :keymap ceamx-cryption-prefix-map
    "d" (cons "decrypt..." (define-prefix-command 'ceamx-cryption-d-prefix))
    "d d" #'epa-decrypt-file
    "d r" #'epa-decrypt-region
    "e" (cons "encrypt..." (define-prefix-command 'ceamx-cryption-e-prefix))
    "e e" #'epa-encrypt-file
    "e r" #'epa-encrypt-region
    "k" #'epa-list-keys)

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
;; :PROPERTIES:
;; :ID:       2923efc3-131c-4cdc-b788-68bfe3cbf256
;; :END:


(use-feature! goto-addr
  :hook (prog-mode . goto-address-prog-mode))

;; =link-hint=: Activate links in buffer with ~avy~

;; <https://github.com/noctuid/link-hint.el>


(package! link-hint
  (define-keymap :keymap (current-global-map)
    "M-g u" #'link-hint-open-link
    "M-g U" #'link-hint-copy-link))

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


(use-feature! ibuffer
  :config
  (setopt ibuffer-movement-cycle t))

;; Rebind some default ~help-map~ keybindings :keybinds:


(define-keymap :keymap help-map
  "K" (cons "[ KEYBINDINGS ]" #'ceamx-help-keybindings-prefix)
  "l" #'find-library
  ;; I prefer the default `man' over `consult-man'.  The latter does
  ;; not really work well since it does not show unfiltered candidates
  ;; by default, and two-character-named programs are too short to
  ;; trigger the query.
  "m" #'man                             ; orig: `describe-mode'
  "M" #'describe-mode
  "t" #'describe-text-properties

  ;; Allow `which-key' pagination in `help-map'.
  "C-h" nil)

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
    (require 'helpful))

  (define-keymap :keymap help-map
    "c" #'helpful-callable
    "C" #'helpful-command
    "f" #'helpful-function              ; orig: `describe-face'
    "F" #'describe-face
    "h" #'helpful-at-point
    "K K" #'helpful-key
    "o" #'helpful-symbol
    "v" #'helpful-variable))

;; DISABLED Keybinding help via =which-key=
;; :PROPERTIES:
;; :ID:       0eb1dafe-55fb-4658-ac9b-53597da45bb3
;; :END:
;; :LOGBOOK:
;; - Refiled on [2025-07-13 Sun 20:07]
;; :END:

;; Currently using ~embark-prefix-help-command~ instead.


(use-feature! which-key
  ;; :hook (ceamx-after-init . which-key-mode)

  :config
  (setopt which-key-compute-remaps t)
  (setopt which-key-idle-delay 1.0)
  (setopt
   which-key-sort-order 'which-key-description-order
   ;; which-key-sort-order 'which-key-prefix-then-key-order
   which-key-sort-uppercase-first nil)
  (setopt which-key-add-column-padding 0)
  (setopt which-key-show-remaining-keys t))

;; Record some variables' values with ~savehist~
;; :PROPERTIES:
;; :ID:       fb765b69-118b-4ef7-8902-f09215137f5c
;; :END:


(use-feature! savehist
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
  :init
  (save-place-mode))

;; Record recently-accessed files with ~recentf~


(use-feature! recentf
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

;; =undo-fu=: Support optional linear undo/redo
;; :PROPERTIES:
;; :ID:       06ac0daa-058d-4124-9fa4-7b891aa8715f
;; :END:

;; - Source code :: <https://codeberg.org/ideasman42/emacs-undo-fu>


(package! undo-fu
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo))

;; =undo-fu-session=: Record undo/redo steps across Emacs sessions

;; - Source code :: <https://codeberg.org/ideasman42/emacs-undo-fu-session>

;; NOTE: This is *NOT* just for use with ~undo-fu~!  It's an essential
;; enhancement to the builtin Emacs undo system as well.


(defvar undo-fu-session-directory
  (expand-file-name "undo-fu-session" ceamx-var-dir))

(package! undo-fu-session
  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setopt undo-fu-session-ignore-temp-files t)
  (setopt undo-fu-session-ignore-encrypted-files t)

  (setopt undo-fu-session-compression 'zst)

  (undo-fu-session-global-mode))

;; =vundo=: Visualize the Emacs undo tree
;; :PROPERTIES:
;; :ID:       8fd3c7aa-cf9d-47f9-91cf-715e6f5d0618
;; :END:

;; - Source code :: <https://github.com/casouri/vundo>


(use-package vundo
  ;; :ensure t
  :defer t
  :defines vundo-unicode-symbols

  :bind
  ("C-x u" . vundo)

  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;; =bookmark-in-project= :: Functions to limit bookmark queries to current project
;; :PROPERTIES:
;; :ID:       ad886f04-b66b-453b-91c6-b1f9502e4af2
;; :END:


(package! (bookmark-in-project
           :host codeberg
           :repo "ideasman42/emacs-bookmark-in-project")

  (define-keymap :keymap ceamx-bookmark-prefix-map
    "b" #'bookmark-in-project-jump
    "n" #'bookmark-in-project-jump-next
    "p" #'bookmark-in-project-jump-previous
    "*" #'bookmark-in-project-toggle))

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


(add-hook 'ceamx-after-init-hook #'repeat-mode)

(after! repeat
  (setopt repeat-exit-timeout 10)
  (setopt repeat-on-final-keystroke t)
  (setopt repeat-keep-prefix t)
  (setopt repeat-exit-key "<return>"))



;; Related, but not technically part of ~repeat-mode~:


(setopt set-mark-command-repeat-pop t)

;; Provide ~ceamx-init-essentials~ feature


(provide 'ceamx-init-essentials)
;;; ceamx-init-essentials.el ends here
