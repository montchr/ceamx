;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

;; Set up the ~ceamx-modeline~


(line-number-mode 1)
(column-number-mode 1)

(setopt display-time-24hr-format t)

(use-feature! ceamx-modeline
  :hook (ceamx-after-init . ceamx-modeline-mode)
  :init
  (keymap-set ceamx-toggle-prefix "m" #'ceamx-modeline-mode)
  :config
  (setq-default mode-line-format
                '("%e"
                  ceamx-modeline-kbd-macro
                  ceamx-modeline-narrow
                  ceamx-modeline-buffer-status
                  ceamx-modeline-window-dedicated-status
                  "  "
                  ceamx-modeline-buffer-identification
                  "  "
                  ceamx-modeline-major-mode
                  ceamx-modeline-process
                  "  "
                  ceamx-modeline-vc-branch
                  "  "
                  ceamx-modeline-eglot
                  "  "
                  ;; ceamx-modeline-flymake
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  ;; ceamx-modeline-notmuch-indicator
                  "  "
                  ceamx-modeline-misc-info)))

;; ~minions~ :: minimize many mode-line minor-modes


(package! minions
  (add-hook 'ceamx-after-init-hook #'minions-mode))

;; ~keycast~ :: show current command and its binding :present:

;; - Website :: <https://github.com/tarsius/keycast>

;; Supports display in the mode-line, header-line, tab-bar, and as
;; messages in a dedicated frame.

;; NOTE: Incompatible with kitchen-sink modeline packages like
;; =doom-modeline= and =telephone-line=.


(after! keycast
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'ceamx-init-modeline)
;;; ceamx-init-modeline.el ends here
