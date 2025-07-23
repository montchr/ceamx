nix-mode nix-ts-mode

(lines "''" n> r> n> "'';")
(mkopt "mkOption {" n> "type = with types; " p ";" n> "default = " p ";" n> "description = ''" n> p n "'';" n "};")
(mkoptenable "enable = lib.mkEnableOption \"" p "\"; " %)

(inherit "inherit (" p ") " p ";")
(istdenv "inherit (pkgs.stdenv.hostPlatform) " p ";" q)

(let "let" n> r> q n "in ")
(doc "/**" n> > r> " :: " p " -> " p n> > "*/")

(mod (i modargs) n "{" n> q n "}")
(modargs "{ config, lib, pkgs, ... }:" o)

(fpackage "flake.perSystem.packages." q)

(fmod & "flake.modules." (s type) "." (s name) " = " (p "
  { pkgs, ... }:
") > "{" n> r n> "};" n)
(persys & "\
  perSystem =
    { " (p "inputs', ") (p "pkgs")", ... }:
    {" n> > r> n> "};" %)

(withsys & "withSystem " (p "system") " ({ " (p "inputs', config, ") "pkgs, ... }: {" n r> n "})")
