;;; templates/nix-mode.eld

nix-mode nix-ts-mode

(inhr "inherit (" p ") " p ";")
(letin "let" n> r> q n "in ")
;; FIXME: indentation wrong
(comm "/*" n> > r> " :: " p " -> " p n> > "*/")

(modargs "{ " (r "config, lib, pkgs") ", ... }:" o)
