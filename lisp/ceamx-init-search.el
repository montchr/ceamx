;; -*- lexical-binding: t -*-

(require 'ceamx-lib)

;; Baseline search configuration


(use-feature! emacs
  :config
  (setopt find-library-include-other-files nil)

  (keymap-set search-map "r" '("replace..." . ceamx-replace-prefix)))

;; Configure interactive searching with ~isearch~


(after! isearch
  (blackout 'isearch)

  ;;
  ;; Settings

  (setopt search-highlight t
          isearch-lazy-highlight t
          lazy-highlight-initial-delay 0.5
          lazy-highlight-no-delay-length 4)
  (setopt isearch-lazy-count t
          lazy-count-prefix-format "[%s/%s] "
          lazy-count-suffix-format nil)
  (setopt isearch-allow-scroll 'unlimited)
  (setopt isearch-wrap-pause t
          isearch-repeat-on-direction-change t)

  ;; Allow extending search string by holding shift and using motion
  ;; commands.
  (setopt isearch-yank-on-move 'shift)

  (setopt search-whitespace-regexp ".*?"
          isearch-lax-whitespace t
          isearch-regexp-lax-whitespace nil)

  ;;
  ;; Keybindings

  (defvar-keymap isearch-repeat-map
    :repeat t
    "s" #'isearch-repeat-forward
    "r" #'isearch-repeat-backward)

  (define-keymap :keymap (current-global-map)
    "M-s M-o" #'multi-occur)

  (define-keymap :keymap isearch-mode-map
    "M-<" #'isearch-beginning-of-buffer
    "M->" #'isearch-end-of-buffer
    "M-/" #'isearch-complete
    "M-w" #'isearch-yank-word-or-char

    "M-s <" #'isearch-beginning-of-buffer
    "M-s >" #'isearch-end-of-buffer

    "C-g" #'isearch-cancel              ; orig. `isearch-abort'

    "C-w" nil
    "M-e" nil)

  (keymap-set minibuffer-local-isearch-map "M-/" #'isearch-complete-edit))

;; Customize the ~occur~ buffer


(after! isearch
  (add-hook 'occur-mode-hook #'ceamx-truncate-lines-silently)

  (setopt list-matching-lines-jump-to-current-line nil)

  (keymap-set occur-mode-map "t" #'toggle-truncate-lines))

;; ~substitute~: efficiently replace contextual targets

;; <https://protesilaos.com/emacs/substitute>


(package! substitute
  (define-keymap :keymap ceamx-replace-prefix
    "b" #'substitute-target-in-buffer
    "d" #'substitute-target-in-defun
    "r" #'substitute-target-above-point
    "s" #'substitute-target-below-point)

  (setopt substitute-hightlight t))

(after! substitute
  ;; Provide messages reporting on matches changed in the context.
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))

;; ~wgrep~: writable grep buffers

;; + Package :: <https://github.com/mhayashi1120/Emacs-wgrep>


(use-package wgrep
  ;; :ensure t

  :bind
  ((nil . nil)
   :map dired-mode-map
   ("C-c C-e" . wgrep-change-to-wgrep-mode)
   :map grep-mode-map
   ("W" . wgrep-change-to-wgrep-mode))

  :config
  (setopt wgrep-auto-save-buffer t))

;; Configure ~re-builder~, the regular expression builder

;; <https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder>
;; <https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/>

;; Unfortunately, ~re-builder~ itself is poorly-documented.


(use-feature! re-builder
  :config
  ;; "string" => recommended: \\(foo\\\|bar\\)
  ;; "rx"     => recommended; advanced sexp regexp engine
  ;; "read"   => default, avoid: backslash hell
  (setopt reb-re-syntax 'string))

;; Configure ~xref~


(defvar xref-ignored-files '()
  "List of files to be ignored by `xref'.")

(after! xref
  (setopt xref-file-name-display 'project-relative)
  (setopt xref-prompt-for-identifier nil)
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setopt xref-show-xrefs-function #'xref-show-definitions-buffer))

;; Provide ~ceamx-init-search~ feature


(provide 'ceamx-init-search)
;;; ceamx-init-search.el ends here
