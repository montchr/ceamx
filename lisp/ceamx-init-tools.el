;; -*- lexical-binding: t;  -*-

(require 'ceamx-lib)
(require 'seq)

;; Set up ~eshell~, the Emacs-Lisp shell


(after! eshell
  (setopt eshell-scroll-to-bottom-on-input 'this))

;; ~eat~ :: [E]mulate [A] [T]erminal
;; :PROPERTIES:
;; :ID:       cafaa1b0-d633-4e7e-b470-6dbfd534c35f
;; :END:

;; + Package :: <https://codeberg.org/akib/emacs-eat/>


(package! eat
  (keymap-set ceamx-launch-prefix "t" #'eat)

  (after! eshell
    (add-hook 'eshell-load-hook #'eat-eshell-mode)
    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))
  (after! popper
    (setopt popper-reference-buffers
            (append popper-reference-buffers '("\\*eat\\*")))))

;; [[https://joostkremers.github.io/pandoc-mode/][Pandoc-mode]]: filetype conversion multitool


(package! pandoc-mode
  (add-hook 'markdown-mode-hook #'pandoc-mode)

  (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings))

;; ~verb~ :: organize and send http requests

;; + Package :: https://github.com/federicotdn/verb


(package! verb
  (after! org
    ;; FIXME: is there a better way to bind `verb-command-map', the
    ;; primary entrypoint?
    ;; (defer! 2
    ;;   (require 'verb))
    (keymap-set org-mode-map "C-c C-r" verb-command-map) ; orig. `org-fold-reveal'
    ))

;; [[https://github.com/hniksic/emacs-htmlize][hniksic/emacs-htmlize]]: Convert buffer text and decorations to HTML


(package! htmlize
  ;; FIXME: conflicts with `beframe'
  ;; (keymap-global-set "C-c b h" #'htmlize-buffer)
  )

;; [[https://github.com/alphapapa/unpackaged.el][alphapapa/unpackaged.el]]: a library of useful yet "unsubstantial" Emacs Lisp code


(package! (unpackaged :host github :repo "alphapapa/unpackaged.el"))

;; ~mugur~: a configurator for QMK keyboards


(package! mugur)

;; ~free-keys~: Show free keybindings for modkeys or prefixes

;; + Package :: <https://github.com/Fuco1/free-keys>


(package! free-keys)

;; ~help-find~ :: introspect keybindings


(package! help-find
  (define-keymap :keymap help-map
    "K b" #'help-find-keybinding
    "K f" #'help-find-function))

;; ~uuidgen~: Generate and insert UUIDs
;; :PROPERTIES:
;; :ID:       48fbfb99-55c9-44db-a342-2c9fe847e85e
;; :END:


(package! uuidgen
  (define-keymap :keymap ceamx-insert-prefix
    "U" (cons "uuid" (define-prefix-command 'ceamx-insert-uuid-prefix))
    "U 1" #'uuidgen-1
    "U 3" #'uuidgen-3
    "U 4" #'uuidgen-4
    "U 5" #'uuidgen-5))

;; Define important feature paths :paths:


(defconst ceamx-ledger-dir (expand-file-name "~/ledger"))
(defconst ceamx-ledger-main-journal-file (file-name-concat ceamx-ledger-dir "main.journal"))

;; Install ~hledger-mode~ :package:


(require 'ceamx-paths)

(package! hledger-mode
  (setopt hledger-jfile ceamx-ledger-main-journal-file))

;; Register ~hledger-mode~ buffers as popups :window:


(after! popper
  (add-to-list 'popper-reference-buffers "\\*Personal Finance\\*"))

;; Register the =hledger= checker for =Flycheck= :checkers:


(package! flycheck-hledger
  (when (fboundp 'flycheck-mode)
    (add-hook 'hledger-mode-hook #'flycheck-mode))

  (setopt flycheck-hledger-strict t))

;; PDF-Tools

;; - website :: <https://github.com/vedang/pdf-tools>
;; - ref :: <https://github.com/jwiegley/dot-emacs/blob/master/init.org>

;; ~pdf-tools~ should be installed installed via Nixpkgs because it requires
;; some separate binaries.


(require 'ceamx-lib)

(defvar pdf-tools-handle-upgrades nil)

(after! pdf-tools
  (dolist
      (pkg
       '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
         pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
         pdf-util pdf-view pdf-virtual))
    (require pkg))
  (pdf-tools-install))

;; ~ready-player~ :: multimedia file previews :nixpkgs:

;; + Package :: <https://github.com/xenodium/ready-player>
;; + Dependencies :: =ffmpeg= =ffmpegthumbnailer= =mpv=

;; This should be installed via Nixpkgs due to external dependencies.


(use-feature! ready-player
  :mode
  ("\\.\\(aiff\\|flac\\|mp3\\|m4a\\|mp4\\|mkv\\|opus\\|wav\\|webm\\)\\'" . ready-player-major-mode)
  :config
  (setopt ready-player-autoplay nil
          ready-player-repeat nil
          ready-player-shuffle nil))

;; emacswiki:i-ching.el

;; + src :: https://www.emacswiki.org/emacs/i-ching.el

;; This package is generally more *interesting* than the simpler and
;; more-discoverable [[https://github.com/zzkt/i-ching][=i-ching= package on MELPA]].  It also, most
;; importantly, supports the display of commentaries on individual
;; changing lines.



(use-feature! i-ching
  :commands (i-ching/lookup
             i-ching/cast))

;; Operate on buffers rectangularly with the ~rect~ feature
;; :PROPERTIES:
;; :ID:       8465808e-c79f-4e30-a88f-a9e64401ff95
;; :END:
;; :LOGBOOK:
;; - Refiled on [2025-01-27 Mon 09:34]
;; :END:


(use-feature! ceamx-tools
  :bind ( "C-x SPC" . #'ceamx/rectangle-dispatch/body))

(provide 'ceamx-init-tools)
;;; ceamx-init-tools.el ends here
