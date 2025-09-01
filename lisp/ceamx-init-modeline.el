;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

;; =mlscroll= :: scrollbar for the modeline


(package! mlscroll
  (mlscroll-mode 1))

;; Set up the ~ceamx-modeline~
;; :PROPERTIES:
;; :END:


(line-number-mode 1)
(column-number-mode 1)

(setq! display-time-24hr-format t)

;; Handled manually.
(setq! notmuch-indicator-add-to-mode-line-misc-info nil)

(use-feature! ceamx-modeline
  :hook (ceamx-after-init . ceamx-modeline-mode)
  :config
  (setq-default mode-line-format
                '("%e"
                  ceamx-modeline-kbd-macro
                  ceamx-modeline-narrow
                  ceamx-modeline-remote-status
                  ceamx-modeline-window-dedicated-status
                  " "
                  ceamx-modeline-buffer-identification
                  " "
                  ceamx-modeline-major-mode
                  " "
                  ceamx-modeline-position
                  ceamx-modeline-process
                  " "
                  (project-mode-line project-mode-line-format)
                  ceamx-modeline-vc-branch
                  " "
                  ceamx-modeline-eglot
                  " "
                  ;; ceamx-modeline-flymake
                  " "
                  mode-line-format-right-align ; Emacs 30
                  mode-line-modes
                  " "
                  ceamx-modeline-format-on-save
                  ceamx-modeline-notmuch-indicator
                  " "
                  ceamx-modeline-misc-info
                  ;;ceamx-modeline-scrollbar
                  )))

;; =minions= :: minimize many mode-line minor-modes


(package! minions
  (add-hook 'ceamx-after-init-hook #'minions-mode)
  ;; NOTE: This must happen after activating `ceamx-modeline-mode'
  ;; because it needs to override the existing modeline format.
  (after! ceamx-modeline
    (add-hook 'ceamx-modeline-mode-hook #'minions-mode)))

;; =keycast= :: show current command and its binding :present:

;; - Website :: <https://github.com/tarsius/keycast>

;; Supports display in the mode-line, header-line, tab-bar, and as
;; messages in a dedicated frame.

;; NOTE: Incompatible with kitchen-sink modeline packages like
;; =doom-modeline= and =telephone-line=.


(package! keycast)

(provide 'ceamx-init-modeline)
;;; ceamx-init-modeline.el ends here
