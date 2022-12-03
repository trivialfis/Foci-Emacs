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
  # Use docker-tramp for emacs 28.x, for newer emacs, builtin tramp container should suffice
  # docker-tramp
  vterm
  winum
  dockerfile-mode
  font-lock-studio
  all-the-icons-dired
  # company
  company
  company-box
  company-posframe
  company-quickhelp
  company-shell
  company-math
  # magit
  magit
  forge
  magit-delta
  # helm
  helm
  helm-gtags
  helm-xref
  # flyspell
  flyspell-correct
  flyspell-correct-helm
  # langs
  nix-mode
  json-mode
  groovy-mode
  cmake-mode
  company-auctex                # Tex
  company-c-headers             # C
  slime                         # Lisp
  slime-company                 # Lisp
  elpy                          # python
  # lsp mode
  lsp-mode
  lsp-ui
  # lsp ext
  lsp-treemacs
  lsp-docker
  lsp-origami
  dap-mode
  # lsp langs
  lsp-metals
  lsp-haskell
  lsp-java
  # treemacs
  treemacs
]))
