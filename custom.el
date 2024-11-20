(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-directories '("/home/cdom/.config/emacs/"))
 '(safe-local-variable-values
    '((apheleia-formatter . nixfmt)
       (eval add-hook 'after-save-hook #'org-gfm-export-to-markdown t t)
       (org-refile-targets (nil :maxlevel . 4))
       (eval load-file "./ceamx-dev-loader.el")
       (lentic-init . lentic-orgel-org-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#000000")))
 '(tab-bar-tab ((t :box (:line-width 2 :color "#000000"))))
 '(tab-bar-tab-inactive ((t :box (:line-width 2 :color "#545454"))))
 '(window-divider ((t :background "#000000" :foreground "#000000")))
 '(window-divider-first-pixel ((t :background "#000000" :foreground "#000000")))
 '(window-divider-last-pixel ((t :background "#000000" :foreground "#000000"))))
