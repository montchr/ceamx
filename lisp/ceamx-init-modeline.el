;; -*- lexical-binding: t; -*-

(require 'ceamx-lib)

;; =mlscroll= :: scrollbar for the modeline


(package! mlscroll
  (mlscroll-mode 1))

;; Set up the ~ceamx-modeline~
;; :PROPERTIES:
;; :ID:       e8fa729f-3a84-4b3b-88eb-450406dde0a5
;; :END:


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
                  ;; ceamx-modeline-notmuch-indicator
                  " "
                  ceamx-modeline-misc-info
                  ;;ceamx-modeline-scrollbar
                  )))

;; =minions= :: minimize many mode-line minor-modes
;; :PROPERTIES:
;; :ID:       af187c9b-e6b2-47dc-a1b0-848a38455d74
;; :END:


(package! minions
  (add-hook 'ceamx-after-init-hook #'minions-mode)
  ;; NOTE: This must happen after activating `ceamx-modeline-mode'
  ;; because it needs to override the existing modeline format.
  (after! ceamx-modeline
    (add-hook 'ceamx-modeline-mode-hook #'minions-mode)))

;; =keycast= :: show current command and its binding :present:
;; :PROPERTIES:
;; :ID:       a205c8de-e065-4d67-9d81-a02c127ef068
;; :END:

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
