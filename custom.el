(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-matching-paren-highlight-offscreen t)
 '(casual-lib-use-unicode nil)
 '(colon-double-space t)
 '(comment-multi-line t)
 '(fill-individual-varying-indent t)
 '(fill-nobreak-predicate
    '(fill-french-nobreak-p fill-single-word-nobreak-p fill-single-char-nobreak-p))
 '(global-mark-ring-max 32)
 '(hscroll-step 0.2)
 '(imenu-max-items 50)
 '(isearch-repeat-on-direction-change t)
 '(kill-whole-line t)
 '(mark-ring-max 32)
 '(mouse-1-double-click-prefer-symbols t)
 '(mouse-avoidance-mode 'animate nil (avoid))
 '(mouse-drag-and-drop-region t)
 '(mouse-drag-and-drop-region-cut-when-buffers-differ t)
 '(mouse-drag-mode-line-buffer t)
 '(mouse-highlight 1)
 '(mouse-wheel-scroll-amount-horizontal 0.25)
 '(next-screen-context-lines 4)
 '(pixel-scroll-precision-mode t)
 '(safe-local-variable-values
    '((eval add-hook 'after-save-hook #'org-gfm-export-to-markdown t t)
       (org-modern-hide-stars) (org-modern-keyword) (org-modern-todo)
       (org-pretty-entities) (magit-todos-exclude-globs ".git/" "lisp/*.el")
       (org-refile-targets ("config.org" :maxlevel . 2) (nil :maxlevel . 4))
       (org-refile-targets (nil :maxlevel . 4))
       (eval load-file "./ceamx-dev-loader.el") (apheleia-formatter . alejandra)
       (apheleia-formatter . nixfmt)
       (org-archive-location . "graveyard/%s.archive::datetree/")
       (lentic-init . lentic-orgel-org-init)))
 '(save-interprogram-paste-before-kill t)
 '(scroll-preserve-screen-position 1)
 '(show-paren-context-when-offscreen 'overlay)
 '(show-paren-ring-bell-on-mismatch t)
 '(show-paren-when-point-in-periphery t)
 '(tempel-trigger-prefix nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#fbf7f0")))
 '(tab-bar-tab ((t :box (:line-width 2 :color "#fbf7f0"))))
 '(tab-bar-tab-inactive ((t :box (:line-width 2 :color "#c8b8b2"))))
 '(window-divider ((t :background "#fbf7f0" :foreground "#fbf7f0")))
 '(window-divider-first-pixel ((t :background "#fbf7f0" :foreground "#fbf7f0")))
 '(window-divider-last-pixel ((t :background "#fbf7f0" :foreground "#fbf7f0"))))
