# .config/nixpkgs/config.nix
# {
#   packageOverrides = pkgs: with pkgs; {
#     fociEmacs = pkgs.callPackage ~/.emacs.d/focus.nix pkgs;
#   };
# }
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
