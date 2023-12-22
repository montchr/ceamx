;;; templates/nix-mode.eld

nix-mode

(inherit "inherit (" p ") " p ";")
(letin "let" n> r> n "in ")
;; FIXME: indentation wrong
(comm "/*" n> > r> " :: " p " -> " p n> > "*/")
