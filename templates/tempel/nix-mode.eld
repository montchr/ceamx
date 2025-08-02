nix-mode nix-ts-mode

(mod (i modargs) n "{" n> q n "}")

(fmod & "flake.modules." (s type) "." (s name) " = " (p "
  { pkgs, ... }:
") > "{" n> r n> "};" n)
