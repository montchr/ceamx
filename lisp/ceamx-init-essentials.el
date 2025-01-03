;; Baseline configurations


(use-feature! emacs
  :config
  (define-keymap :keymap (current-global-map)
    "M-c" #'capitalize-dwim
    "M-f" #'forward-word
    "M-F" #'forward-symbol
    "M-l" #'downcase-dwim
    "M-o" #'delete-blank-lines
    "M-Q" #'repunctuate-sentences
    "M-u" #'upcase-dwim
    "M-z" #'zap-up-to-char              ; orig: `zap-to-char'
    "M-=" #'count-words
    "M-SPC" #'cycle-spacing

    "C-h F" #'apropos-function
    "C-h L" #'apropos-library
    "C-h U" #'apropos-user-option
    "C-h V" #'apropos-variable

    ;; TODO: move to window config
    "C-x O" #'next-multiframe-window)

  ;; Keymap for buffers
  ;; TODO: copy some of these to `ceamx-toggle-prefix'
  (define-keymap :keymap ctl-x-x-map
    "f" #'follow-mode
    "l" #'visual-line-mode
    "r" #'rename-uniquely)

  (define-keymap :keymap prog-mode-map
    ;; Move forward out of one sexp level
    "C-M-d" #'up-list))

;; ~ceamx-simple~: Simple & common commands


(use-feature! ceamx-simple
  :demand t
  :config
  (define-keymap :keymap (current-global-map)
    "C-x k" #'ceamx-simple/kill-current-buffer ; orig: `kill-buffer'
    "C-x K" #'kill-buffer

    ;; FIXME: move defun to `ceamx-simple'
    "M-DEL" #'ceamx/backward-kill-word

    "C-M-SPC" #'ceamx-simple/mark-sexp

    ;; Commands for lines
    ;; TODO: currently `easy-kill'
    ;; "M-w" #'ceamx-simple/kill-ring-save
    "M-k" #'ceamx-simple/kill-line-backward
    ;; TODO: currently `avy-goto-char-timer'
    ;; "M-j" #'delete-indentation
    "C-S-d" #'ceamx-simple/duplicate-line-or-region
    ;; TODO: redundant with `easy-kill'
    "C-S-w" #'ceamx-simple/copy-line
    "C-S-y" #'ceamx-simple/yank-replace-line-or-region
    ;; FIXME: these have weird quirks esp. in folded org-mode buffers
    ;; "C-v" #'ceamx-simple/multi-line-below ; orig: `scroll-up-command'
    ;; "<next>" #'ceamx-simple/multi-line-below ; orig: `scroll-up-command'
    ;; "M-v" #'ceamx-simple/multi-line-above ; orig: `scroll-down-command'
    ;; "<prior>" #'ceamx-simple/multi-line-above ; orig: `scroll-down-command'
    "C-RET" #'ceamx-simple/new-line-below
    "C-S-RET" #'ceamx-simple/new-line-above

    ;; Commands for text insertion or manipulation
    "C-<" #'ceamx-simple/escape-url-dwim
    "M-Z" #'ceamx-simple/zap-to-char-backward

    ;; Commands for buffers
    "M-s b" #'ceamx-simple/buffers-major-mode
    "M-s v" #'ceamx-simple/buffers-vc-root)

  (keymap-substitute (current-global-map) #'default-indent-new-line #'ceamx-simple/continue-comment))

;; Configure sane window-scrolling behavior


(use-feature! window
  :bind
  ("C-x <" . scroll-right)
  ("C-x >" . scroll-left)
  ("<wheel-left>" . scroll-left)
  ("<wheel-right>" . scroll-right)

  :config
  ;; Available cycle positions for `recenter-top-bottom'.
  (setopt recenter-positions '(top middle bottom))

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

;; Whitespace and indentation

;; Normalize whitespace and indentation handling:


(use-feature! emacs
  :hook ((before-save . delete-trailing-whitespace))

  :config
  (setq-default indent-tabs-mode nil
                tab-width 8)

  (setopt backward-delete-char-untabify-method 'hungry)
  (setopt mode-require-final-newline 'visit-save)
  (setopt sentence-end-double-space t)

  (electric-indent-mode 1))



;; Visualize notable and unusual whitespace:


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

;; Enforce EditorConfig settings

;; - website :: <https://editorconfig.org>


(use-package editorconfig
  :ensure t
  :hook (ceamx-emacs-startup . editorconfig-mode)

  :preface
  ;; via <https://github.com/doomemacs/doomemacs/commit/43870bf8318f6471c4ce5e14565c9f0a3fb6e368>
  (defun +editorconfig-enforce-org-mode-tab-width-h (props)
  "Prevent `editorconfig' from changing `tab-width' in `org-mode'.
A \"tab-width\" of any value other than 8 is an error state in
org-mode, so it must not be changed.

PROPS is as in `editorconfig-after-apply-functions'."
  (when (and (gethash 'indent_size props)
             (derived-mode-p 'org-mode))
    (setq tab-width 8)))

  :config
  (add-hook 'editorconfig-after-apply-functions #'+editorconfig-enforce-org-mode-tab-width-h))

;; ~mwim~: Replace ~beginning-of-line~ and ~end-of-line~ with DWIM alternatives


(package! mwim
  ;; FIXME: overrides `org-mode' bindings!
  (keymap-global-set "C-a" #'mwim-beginning)
  (keymap-global-set "C-e" #'mwim-end))

;; INPRG Provide a command to intelligently kill words backwardsly

;; - State "INPRG"      from "TODO"       [2024-07-13 Sat 22:02] \\
;;   Needs a fix for compatibility with ~subword-mode~.  See also [[*Don't consider camelCaseWORDs as separate words]]
;; - src :: https://www.reddit.com/r/emacs/comments/bz9rxn/comment/er0bgll/
;; - src :: https://github.com/yantar92/emacs-config/blob/master/config.org#smarter-backward-kill-word


(defun ceamx/backward-kill-word ()
  "Kill the previous word, smartly.
This operation will respect the following rules:

1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is *only* whitespace, delete only to beginning of line.
3. If there is *some* whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)
  (if (bolp)
      ;; 1
      (delete-char -1)
    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        ;; 2
        (delete-horizontal-space)
      (when (thing-at-point 'whitespace)
        ;; 3
        (delete-horizontal-space))

      (if (thing-at-point 'word)
          ;; 5
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (point)))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        ;; 4
        (delete-char -1)))))

;; ~easy-kill~ :package:

;; + Package documentation :: <https://github.com/leoliu/easy-kill/blob/master/README.rst>

;; #+begin_example
;; w => word
;; s => sexp
;; l => list
;; d => defun
;; D => defun name
;; f => file
;; b => buffer name
;;        ->"-": `default-directory'
;;        ->"+": full path
;;        ->"0": basename
;; #+end_example


(use-package easy-kill
  :ensure t
  :commands (easy-kill easy-mark)
  :init
  (keymap-global-set "M-w" #'easy-kill)   ; override `kill-ring-save'
  (keymap-global-set "C-M-@" #'easy-mark) ; override `mark-sexp'
  )

;; Replace region when inserting text


(delete-selection-mode 1)

;; ~expand-region~: Increase/decrease the selection area


(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :init
  (keymap-global-set "C-=" #'er/expand-region))

;; ~drag-stuff~: drag stuff around in arbitrary directions :package:

;; <https://github.com/rejeep/drag-stuff.el>

;; This package appears to be abandoned since 2017.  As of <2024-12-27>,
;; it still works relatively well, but has some issues:

;; + Possible subtle conflicts with ~org-metaup~ and ~org-metadown~?
;; + Numerous warnings about deprecated functions <https://github.com/rejeep/drag-stuff.el/issues/36>

;; I haven't yet found any other package to move arbitrary regions
;; up/down while preserving column position.

;; ~move-text-mode~ <https://github.com/emacsfodder/move-text> claims to do
;; this, but fails pretty badly, moving the region/selection to the first
;; column regardless of its original position.


(use-package drag-stuff
  :ensure t
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

;; Don't consider camelCaseWORDs as separate words

;; While it can be useful in some contexts, I wish that ~subword-mode~ did not break
;; ~ceamx/backward-kill-word~.  See also [[*Provide a command to intelligently kill
;; words backwardsly]]


(global-subword-mode -1)

;; TODO ~string-inflection~: Commands to cycle through word casing

;; Needs better bindings.


(require 'lib-editor)

(package! string-inflection)

(defvar-keymap ceamx-string-repeat-map
  :repeat t

  "c" #'ceamx/cycle-string-inflection)

(defun ceamx/cycle-string-inflection ()
  "Cycle through `string-inflection' styles appropriate to the major-mode."
  (interactive)
  (pcase major-mode
    (`emacs-lisp-mode (string-inflection-all-cycle))
    (`python-mode (string-inflection-python-style-cycle))
    (`java-mode (string-inflection-java-style-cycle))
    (`elixir-mode (string-inflection-elixir-style-cycle))
    (_ (string-inflection-ruby-style-cycle))))

;; ~ialign~: Interactively ~align-regexp~ :package:

;; <https://github.com/mkcms/interactive-align/blob/master/README.org#usage>


(package! ialign
  (keymap-global-set "C-x l" #'ialign))

;; ~rect~ [builtin]: operate on a buffer rectangularly

;; <https://github.com/abo-abo/hydra/wiki/Rectangle-Operations#rectangle-2>


(use-feature! rect
  :config
  (use-feature! hydra
    :config
    (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                         :color pink
                                         :hint nil
                                         :post (deactivate-mark))
      "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
      ("k" rectangle-previous-line)
      ("j" rectangle-next-line)
      ("h" rectangle-backward-char)
      ("l" rectangle-forward-char)
      ("d" kill-rectangle)               ;; C-x r k
      ("y" yank-rectangle)               ;; C-x r y
      ("w" copy-rectangle-as-kill)       ;; C-x r M-w
      ("o" open-rectangle)               ;; C-x r o
      ("t" string-rectangle)             ;; C-x r t
      ("c" clear-rectangle)              ;; C-x r c
      ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
      ("N" rectangle-number-lines)            ;; C-x r N
      ("r" (if (region-active-p)
               (deactivate-mark)
             (rectangle-mark-mode 1)))
      ("u" undo nil)
      ("g" nil))

    (when (fboundp 'hydra-rectangle/body)
      (keymap-global-set "C-x SPC" #'hydra-rectangle/body)
      (keymap-global-set "C-x M-r" #'rectangle-mark-mode))))

;; Line wrapping


(use-feature! emacs
  :hook (((prog-mode text-mode) . auto-fill-mode))

  :config
  (setq-default fill-column 70)
  ;; Disable line soft-wrapping by default.
  (setq-default truncate-lines t)

  (setopt comment-auto-fill-only-comments t))

(use-package unfill
  :ensure t
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

;; Use Emacs for =pinentry=


(use-feature! epg
  :defer 2
  :config
  (setopt epg-pinentry-mode 'loopback))

;; Buttonize URLs and email addresses with ~goto-address~ [builtin]


(use-feature! goto-addr
  :hook (prog-mode . goto-address-prog-mode))

;; ~link-hint~: Activate links in buffer with ~avy~

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

;; Auto-save file-visiting buffers


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

;; ~casual-suite~: transient-dispatch menus for complex modes


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

;; ~Info-mode~ enchantments


(use-feature! info
  :hook ((Info-mode . hl-line-mode)
         (Info-mode . scroll-lock-mode)))

;; ~helpful~: Provide improved alternatives to the builtin "describe" utilities

;; - Source code :: <https://github.com/Wilfred/helpful>

;; Note that there is a severe but edge-case bug that has gone unfixed
;; for quite a while.  ~helpful~ cannot display documentation for symbols
;; defined in Emacs C source code:

;; <https://github.com/Wilfred/helpful/issues/329>



(package! helpful
  (defer! 2
    (require 'helpful))

  (define-keymap :keymap help-map
    "c" #'helpful-callable
    "C" #'helpful-command
    "f" #'helpful-function              ; orig: `describe-face'
    "h" #'helpful-at-point
    ;; TODO: consider swapping with the original as a trial?
    "k" #'helpful-key                   ; orig: `describe-key-briefly'
    "o" #'helpful-symbol
    "v" #'helpful-variable

    ;; Parity with the corresponding unmodded keys.
    ;; Primarily for Meow keypad, but also sometimes feels more natural to keep
    ;; holding Ctrl anyway.
    "C-k" #'helpful-key
    "C-o" #'helpful-symbol

    ;; Rebind the originals
    "F" #'describe-face
    "K" #'describe-key-briefly

    ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
    "C-h" nil))

;; Rebind some default ~help-map~ keybindings


(define-keymap :keymap help-map
  "l" #'find-library
  ;; I actually prefer the default `man' over `consult-man'.
  "m" #'man                     ; orig: `describe-mode'
  "M" #'describe-mode

  ;; FIXME: no lambda binding
  ;; "t" `("text-props (pt)" . ,(cmd!!
  ;;                              #'describe-text-properties
  ;;                              current-prefix-arg
  ;;                              (point)))

  ;; Unbind the default binding for "C-h C-h" to allow `which-key' paging.
  "C-h" nil)

;; Record some variables' values with ~savehist~


(use-feature! savehist
  :init
  (savehist-mode)

  :config
  (cl-dolist (save '(kill-ring
                     regexp-search-ring
                     search-ring))
    (cl-pushnew save savehist-additional-variables))
  (setopt history-length 100
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

;; ~undo-fu~: Support optional linear undo/redo

;; - Source code :: <https://codeberg.org/ideasman42/emacs-undo-fu>


(package! undo-fu
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z" #'undo-fu-only-redo))

;; ~undo-fu-session~: Record undo/redo steps across Emacs sessions

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

;; ~vundo~: Visualize the Emacs undo tree

;; - Source code :: <https://github.com/casouri/vundo>


(use-package vundo
  :ensure t
  :defer t
  :defines vundo-unicode-symbols

  :bind
  ("C-x u" . vundo)

  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols))

;; ~dogears~: Return to previously-visited buffer positions

;; - Source code :: <https://github.com/alphapapa/dogears.el>


(package! dogears
  (add-hook 'on-first-buffer-hook #'dogears-mode)

  ;; Also see `ceamx/dogears-dispatch'.
  (define-keymap :keymap (current-global-map)
    ;; TODO: find a new binding maybe
    ;; "M-g d" #'dogears-go
    "M-g M-b" #'dogears-back
    "M-g M-f" #'dogears-forward
    "M-g M-d" #'dogears-list
    "M-g M-D" #'dogears-sidebar)

  ;; Persist `dogears-list' between Emacs sessions.
  ;; via <https://github.com/alphapapa/dogears.el/issues/4>
  (after! savehist
    (when (boundp 'savehist-additional-variables)
      (add-to-list 'savehist-additional-variables #'dogears-list))))

;; TODO: provide a little more context in transient (label for dogears, links maybe...)
(after! (transient dogears)
  (transient-define-prefix ceamx/dogears-dispatch ()
    "Transient menu for `dogears' history navigation commands."
    [["Navigate"
      ("b" "back" dogears-back :transient transient--do-stay)
      ("f" "forward" dogears-forward :transient transient--do-stay)]
     ;; TODO: when quit one of these Find commands, return to transient
     ["Find"
      ("d" "go..." dogears-go)
      ("l" "list" dogears-list)
      ("S" "sidebar" dogears-sidebar)]])

  (defer-until! (fboundp 'ceamx/dogears-dispatch)
    (keymap-global-set "M-g d" #'ceamx/dogears-dispatch)))

;; ~which-key~


(use-feature! which-key
  :hook (ceamx-after-init . which-key-mode)

  :config
  (setopt which-key-compute-remaps t)
  (setopt which-key-idle-delay 1.0)
  (setopt which-key-sort-order 'which-key-prefix-then-key-order
          which-key-sort-uppercase-first nil)

  ;; The default (0) is difficult to read.
  (setopt which-key-add-column-padding 2)

  ;; FIXME: no effect? what does this actually do?
  (setopt which-key-show-remaining-keys t))

;; macOS: Remap modifier keys for the Apple keyboard layout


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

;; Enable and configure ~repeat-mode~


(setopt repeat-exit-timeout 15)
(setopt repeat-on-final-keystroke t)
(setopt repeat-keep-prefix nil)



;; Allow any key sequence to exit ~repeat-mode~:


(setopt repeat-exit-key nil)



;; Related, but not technically part of ~repeat-mode~:


(setopt set-mark-command-repeat-pop t)



;; Enable ~repeat-mode~, avoiding running mode-hooks too early:


(add-hook 'ceamx-after-init-hook #'repeat-mode)

;; Provide ~ceamx-init-essentials~ feature


(provide 'ceamx-init-essentials)
;;; ceamx-init-essentials.el ends here
