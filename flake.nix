{
  description = "ceamx: an emacs config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.apparat.url = "sourcehut:~montchr/apparat";
  inputs.devshell.url = "github:numtide/devshell";

  outputs =
    inputs@{ flake-parts, apparat, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devshell.flakeModule ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem =
        { pkgs, ... }:
        {
          formatter.default = pkgs.nixfmt-rfc-style;
          devshells.default =
            let
              cats = apparat.lib.devshell.categorised [
                "formatters"
                "tools"
              ];
              formatter = cats.formatters.pkg;
              tool = cats.tools.pkg;
            in
            {
              devshell.name = "Ceamx";
              commands = [
                (tool pkgs.just)
                (formatter pkgs.nixfmt-rfc-style)
              ];
              env = [
                {
                  name = "NIXFMT_MAX_WIDTH";
                  value = 80;
                }
              ];
              devshell.packages = [ pkgs.fd ];
            };
        };
      flake = { };
    };

  # NOTE: Retained for provisioning purposes, but normally unnecessary.
  # nixConfig = {
  #   extra-experimental-features = "nix-command flakes";
  #   extra-substituters = [
  #     "https://dotfield.cachix.org"
  #     "https://nix-community.cachix.org"
  #   ];
  #   extra-trusted-public-keys = [
  #     "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
  #     "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  #   ];
  # };
}
