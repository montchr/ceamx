(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
    '((apheleia-formatter . alejandra)
       (org-refile-targets (nil :maxlevel . 6))
       (apheleia-formatter . nixfmt)
       (eval load-file "./ceamx-dev-loader.el")
       (eval add-hook 'after-save-hook #'org-gfm-export-to-markdown t
         t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
