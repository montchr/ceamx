;;;; templates/emacs-lisp-mode.eld

emacs-lisp-mode

(key "(keymap-set " (s map) " \"" (s key) "\" #'" (s cmd) ")" q)
(keys "(define-keymap :keymap " (p "(current-global-map)" map) n>
        "\"" p "\" #'" p ")" q)
(keyg "(keymap-global-set \"" (s key) "\" #'" (s cmd) q ")" q)

(after "(after! " (s feature) n> r ")")

(seto "(setopt " p " " p ")")
(setq "(setq " (s name) " " (s val) ")")
