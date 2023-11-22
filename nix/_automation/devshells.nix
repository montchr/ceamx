{ inputs, cell, }:
let
  l = inputs.nixpkgs.lib // builtins;
  inherit (inputs) nixpkgs std nixfmt;
in l.mapAttrs (_: std.lib.dev.mkShell) {
  default = { ... }: {
    name = "ceamx";
    commands = [
      {
        package = nixpkgs.treefmt;
        category = "repo tools";
      }
      {
        package = nixfmt.packages.default;
        category = "repo tools";
      }
      {
        package = std.std.cli.default;
        category = "repo tools";
      }
    ];
  };
}
