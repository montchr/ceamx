(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(safe-local-variable-values
    '((magit-todos-exclude-globs ".git/" "lisp/*.el")
       (org-refile-targets ("config.org" :maxlevel . 2) (nil :maxlevel . 4))
       (org-refile-targets (nil :maxlevel . 4))
       (eval load-file "./ceamx-dev-loader.el") (apheleia-formatter . alejandra)
       (apheleia-formatter . nixfmt)
       (org-archive-location . "graveyard/%s.archive::datetree/")
       (lentic-init . lentic-orgel-org-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
