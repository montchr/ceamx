;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-unlispify-menu-entries nil)
 '(ignored-local-variable-values
   '((apheleia-inhibit . t) (apheleia-formatter . alejandra)
     (org-refile-targets (nil :maxlevel . 6))
     (eval load-file "./ceamx-dev-loader.el")))
 '(message-citation-line-format "On %a, %b %d %Y, %n wrote:")
 '(package-selected-packages
   '(activities aggressive-indent auto-tangle-mode bookmark-in-project
                breadcrumb cape casual-suite combobulate consult-eglot
                consult-notmuch corfu crux cursory cycle-quotes denote
                denote-journal denote-silo devdocs diff-hl dired-preview
                dired-subtree diredfl doct doric-themes dotenv-mode
                drag-stuff dumb-jump easy-kill eat edit-indirect
                ef-themes elfeed-goodies elfeed-org elfeed-score
                elfeed-tube-mpv elisp-demos embark-consult emmet-mode
                envrc eros exec-path-from-shell exiftool expreg
                fish-mode flycheck-eglot flycheck-phpstan fontaine
                free-keys git-commit-ts-mode git-modes git-timemachine
                gnu-elpa-keyring-update gnugo go golden-ratio gptel
                haskell-mode haskell-ts-mode help-find helpful hl-todo
                hledger-mode htmlize ialign indent-bars jinx jq-mode
                just-ts-mode kanata-kbd-mode kbd-mode kdl-ts-mode
                keycast keymap-utils kind-icon ledger-mode ligature lin
                lispy macrostep magit marginalia markdown-ts-mode
                mastodon mines minions mlscroll modus-themes morlock
                mugur mwim neon-mode nerd-icons-completion
                nerd-icons-dired nix-ts-mode no-littering
                notmuch-indicator olivetti orderless org-appear
                org-download org-modern org-node org-remark org-sidebar
                org-web-tools outli ox-gfm page-break-lines pandoc-mode
                pdf-tools polymode popper prism pulsar puni rainbow-mode
                reformatter rustic savefold setup shift-number show-font
                smart-newline spacious-padding spdx standard-themes
                string-inflection substitute svg-tag-mode systemd tempel
                tmr transpose-frame treesit-auto treesit-fold typo
                undo-fu-session uuidgen vc-jj verb vertico vundo
                web-mode wgrep yaml-pro yasnippet yuck-mode))
 '(package-vc-selected-packages
   '((kbd-mode :url "https://github.com/kmonad/kbd-mode")
     (auto-tangle-mode :url
                       "https://github.com/progfolio/auto-tangle-mode.el")))
 '(safe-local-variable-values
   '((just-ts-indent-offset . 4) (jq-indent-offset . 2)
     (just-ts-indent-offset . 2) (apheleia-formatter . nixfmt)
     (org-archive-location . "graveyard/%s.archive::datetree/")))
 '(send-mail-function 'sendmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
