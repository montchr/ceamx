(use-package dimmer
  :after (modus-themes)

  :custom
  ;; FIXME: include corfu etc
  (dimmer-exclusion-regexp-list '("^\\*[h|H]elm.*\\*"
                                  "^\\*Minibuf-.*\\*"
                                  "^\\*Echo.*"
                                  "^.\\*which-key\\*$"))

  :config
  ;;; See notes re: Modus Themes compatibility:
  ;;; <https://protesilaos.com/emacs/modus-themes#h:8eb4b758-d318-4480-9ead-357a571beb93>
  (setq dimmer-fraction 0.3)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)

  (setq dimmer-watch-frame-focus-events nil)

  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  ;; TODO
  ;; (dimmer-configure-org)
  (dimmer-mode t))
