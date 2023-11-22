{
  description = "ceamx: an emacs config";

  inputs.std.url = "github:divnix/std";
  inputs.std.inputs.nixpkgs.follows = "nixpkgs";

  inputs.nil-lsp.url = "github:oxalica/nil";
  inputs.nixfmt.url = "github:serokell/nixfmt";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { std, self, ... }@inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      cellBlocks = with std.blockTypes; [
        (installables "packages" { ci.build = true; })
        (devshells "devshells" { ci.build = true; })
      ];
    } {
      devShells = std.harvest self [ "_automation" "devshells" ];
      # FIXME: std is non-intuitive how the hell does this work
      # formatter = inputs.nixfmt.packages.default;
      formatter."aarch64-darwin" =
        inputs.nixfmt.packages."aarch64-darwin".default;
    };
}
