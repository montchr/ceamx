lisp-mode emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(autoload ";;;###autoload")

(after "(after! " (s feature) n> r ")")
(key "(keymap-set " (s map) " \"" (s key) "\" #'" (s cmd) ")" q)
(keys "(define-keymap :keymap " (p "(current-global-map)" map) n>
        "\"" p "\" #'" p ")" q)
(keyg "(keymap-global-set \"" (s key) "\" #'" (s cmd) q ")" q)
(seto "(setopt " p " " p ")")
(setq "(setq " (s name) " " (s val) ")")
