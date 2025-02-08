let
  lines = ''
    these are lines
  '';
in
{
  config,
  lib,
  pkgs,
  ...
}:
{
  hello = "world";
}
