;; -*- lexical-binding: t;  -*-

(require 'ceamx-paths)
(require 'ceamx-lib)
(require 'seq)

;; Set up ~eshell~, the Emacs-Lisp shell


(after! eshell
  (setopt eshell-scroll-to-bottom-on-input 'this))

;; =eat= :: [E]mulate [A] [T]erminal
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

;; =verb= :: organize and send http requests

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
;; :PROPERTIES:
;; :ID:       92559e4f-9d4a-4613-82bf-c2373cb5e5da
;; :END:


(package! htmlize
  ;; FIXME: conflicts with `beframe'
  ;; (keymap-global-set "C-c b h" #'htmlize-buffer)
  )

;; [[https://github.com/alphapapa/unpackaged.el][alphapapa/unpackaged.el]]: a library of useful yet "unsubstantial" Emacs Lisp code


(package! (unpackaged :host github :repo "alphapapa/unpackaged.el"))

;; =mugur=: a configurator for QMK keyboards


(package! mugur)

;; =free-keys=: Show free keybindings for modkeys or prefixes

;; + Package :: <https://github.com/Fuco1/free-keys>


(package! free-keys)

;; =help-find= :: introspect keybindings


(package! help-find
  (define-keymap :keymap help-map
    "K b" #'help-find-keybinding
    "K f" #'help-find-function))

;; =uuidgen=: Generate and insert UUIDs
;; :PROPERTIES:
;; :ID:       48fbfb99-55c9-44db-a342-2c9fe847e85e
;; :END:


(package! uuidgen
  (define-keymap :keymap ceamx-insert-prefix-map
    "U" (cons "uuid" (define-prefix-command 'ceamx-insert-uuid-prefix))
    "U 1" #'uuidgen-1
    "U 3" #'uuidgen-3
    "U 4" #'uuidgen-4
    "U 5" #'uuidgen-5))

;; Define important feature paths :paths:
;; :PROPERTIES:
;; :ID:       6af77ff1-5d06-41c8-bedf-f123ae532989
;; :END:


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
;; :PROPERTIES:
;; :ID:       9d793672-7e26-4e2d-9aeb-8aaa85d14128
;; :END:

;; - website :: <https://github.com/vedang/pdf-tools>
;; - ref :: <https://github.com/jwiegley/dot-emacs/blob/master/init.org>

;; ~pdf-tools~ should be installed installed via Nixpkgs because it requires
;; some separate binaries.


(require 'ceamx-lib)

(use-feature! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (dolist
      (pkg '( pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
              pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
              pdf-util pdf-view pdf-virtual))
    (require pkg))

  (def-advice! +pdf-suppress-large-file-prompts-a
      (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    "Silence warnings about PDF filesizes."
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw)))

  (setopt pdf-tools-handle-upgrades nil)
  ;; Resolve the path to required binaries
  ;; FIXME: this should be handled by nixpkgs?
  ;; https://discourse.nixos.org/t/with-an-overlay-can-i-put-the-binary-in-my-path/43759/13
  (setopt pdf-info-epdfinfo-program
          (car (file-expand-wildcards
                (concat "/etc/profiles/per-user/" (getenv "USER")
                        "/share/emacs/site-lisp/elpa/pdf-tools-*/epdfinfo"))))
  ;; HiDPI support
  (setopt pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)

  (setq-default pdf-view-display-size 'fit-page)

  (keymap-set pdf-view-mode-map "q" #'kill-current-buffer)

  (pdf-tools-install))

(after! popper
    (pushnew! popper-reference-buffers
              "^\\*Outline*"
              "^\\*Edit Annotation "
              "\\(?:^\\*Contents\\|'s annots\\*$\\)"))

(after! (pdf-view pdf-annot)
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)
  (add-hook 'pdf-view-mode-hook
            (##add-hook 'kill-buffer-hook #'ceamx-tools-pdf-annot-cleanup-windows-h nil t)))

(package! saveplace-pdf-view
  (after! pdf-view
    (require 'saveplace-pdf-view)))

;; =ready-player= :: multimedia file previews :nixpkgs:
;; :PROPERTIES:
;; :ID:       96e9c541-fc8f-42cf-9f22-b726dc37f7cb
;; :END:

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



;; Provide integration with =dired-preview=:


(use-feature! ceamx-tools
  :after (dired-preview)
  :commands (ceamx/ready-player-dired-preview-play-toggle)
  :config
  (keymap-set dired-preview-mode-map "C-c C-p" #'ceamx/ready-player-dired-preview-play-toggle))

;; =mpdel= :: MPD client for Emacs
;; :PROPERTIES:
;; :ID:       4009635b-4bf9-4b7e-afd8-e829d8ab45a6
;; :END:


(package! mpdel)

;; =linkmarks= :: Keep link bookmarks in Org-Mode
;; :PROPERTIES:
;; :ID:       db54377f-9e4f-4708-a4ce-539e7441423e
;; :END:


(package! (linkmarks :host github :repo "dustinlacewell/linkmarks")
  (setopt linkmarks-file (expand-file-name "bookmarks.org" ceamx-biblio-dir)))

;; =org-capture-ref= :: Extract metadata from websites for capture
;; :PROPERTIES:
;; :ID:       45bc1e57-c0cf-4896-a32f-eb8ade2257c8
;; :END:


;; XXX: Dependency of `org-capture-ref' without recipe.
(package! (persid :host github :repo "rougier/persid"))

(package! (org-capture-ref :host github :repo "yantar92/org-capture-ref")
  (require 'org-capture-ref)

  (setopt org-capture-templates
          (doct-add-to
           org-capture-templates
           `( :group "Weblink"
              :type entry
              :file ,(expand-file-name "links.org" ceamx-biblio-dir)
              :hook ,(##org-capture-ref-process-capture)
              :link-type ,(##org-capture-ref-get-bibtex-field :type)
              :org-entry ,(##org-capture-ref-get-org-entry)
              :template ("* TODO %?%{space}%{org-entry}"
                         ""
                         "+ type :: %{link-type}")
              :children (("Interactive" :keys "L"
                          :clock-in t
                          :clock-resume t
                          :space " ")
                         ("Silent" :keys "l"
                          :space ""
                          :immediate-finish t))))))

;; Bibliographies
;; :PROPERTIES:
;; :ID:       6ffdedb4-5a6a-47bf-98ca-40e3147f7d03
;; :END:


(package! parsebib)
(package! citeproc)

;; `citar'

(package! citar
  (setopt citar-bibliography (list (expand-file-name "master.bib" ceamx-biblio-dir)))
  (setopt org-cite-insert-processor 'citar
          org-cite-follow-processor 'citar
          org-cite-activate-processor 'citar))

(when (locate-library "embark")
  (package! citar-embark
    (after! (citar embark)
      (citar-embark-mode))))

(when (locate-library "denote")
  (package! citar-denote))

;; `org-cite'

;; (after! oc
;;   (require 'oc-biblatex)

;;   (setopt org-cite-global-bibliography citar-bibliography)
;;   (setopt org-cite-export-processors '((latex biblatex) (t csl))))

;; oc-csl requires citeproc, which requires the top-level org, so
;; loading oc-csl after oc interferes with incremental loading of Org.
(after! org
  (require 'oc-csl))

;; yijing.el
;; :PROPERTIES:
;; :ID:       c5573349-c13f-4702-8cf3-8f7cdb49add4
;; :END:

;; + src :: https://www.emacswiki.org/emacs/i-ching.el

;; This package is generally more *interesting* than the simpler and
;; more-discoverable [[https://github.com/zzkt/i-ching][=i-ching= package on MELPA]].  It also, most
;; importantly, supports the display of commentaries on individual
;; changing lines.



(use-feature! yijing
  :commands (yijing/lookup
             yijing/cast))

;; Operate on buffers rectangularly with the ~rect~ feature
;; :PROPERTIES:
;; :ID:       8465808e-c79f-4e30-a88f-a9e64401ff95
;; :END:
;; :LOGBOOK:
;; - Refiled on [2025-01-27 Mon 09:34]
;; :END:


(use-feature! ceamx-tools
  :bind ( "C-x SPC" . #'ceamx/rectangle-dispatch/body))

;; =ceamx-typer= :: Use Emacs anywhere (kinda) :nixpkgs:

;; + Requires-Executable :: =wtype=


(setup ceamx-typer
  (:require))

(provide 'ceamx-init-tools)
;;; ceamx-init-tools.el ends here
