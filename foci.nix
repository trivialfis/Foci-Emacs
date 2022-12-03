# Place following configurations in respected files.
#
# .config/nixpkgs/config.nix
# {
#   packageOverrides = pkgs: with pkgs; {
#     fociEmacs = pkgs.callPackage ~/.emacs.d/foci.nix pkgs;
#   };
# }
#
# .config/nixpkgs/overlays/emacs-packages.nix
# import (builtins.fetchTarball {
#   url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
# })

{ pkgs, ... }:

let
  emacsPgtk = pkgs.emacsPgtk;
  emacsWithPackages = (pkgs.emacsPackagesFor emacsPgtk).emacsWithPackages;
in
emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
  groovy-mode
  # Use docker-tramp for emacs 28.x, for newer emacs, builtin tramp container should suffice
  # docker-tramp
  vterm
  # magit
  magit
  forge
  magit-delta
  # langs
  nix-mode
  json-mode
  # lsp mode
  lsp-mode
  lsp-ui
  # lsp ext
  lsp-treemacs
  lsp-docker
  lsp-origami
  # lsp langs
  lsp-metals
  lsp-haskell
  lsp-java
  dap-mode
]))
