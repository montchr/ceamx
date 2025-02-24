lisp-mode emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(filename (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
(feature (file-name-base (or (buffer-file-name) (buffer-name))))

(autoload ";;;###autoload")

(after "(after! " (s feature) n> r ")")
(key "(keymap-set " (s map) " \"" (s key) "\" #'" (s cmd) ")" q)
(keys "(define-keymap :keymap " (p "(current-global-map)" map) n>
        "\"" p "\" #'" p ")" q)
(keyg "(keymap-global-set \"" (s key) "\" #'" (s cmd) q ")" q)
(seto "(setopt " p " " p ")")
(setq "(setq " (s name) " " (s val) ")")

(header ";;; " (i filename)
        " --- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (i feature) ")" n
         ";;; " (i filename)
         " ends here" n)

(custom & "(defcustom " (i feature) "-" p " " (p "nil") n
        "\"" p "\"" n>
        ":group '" (i feature) n>
        ":type '" p ")")
