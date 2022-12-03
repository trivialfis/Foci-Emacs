# .config/nixpkgs/config.nix
# {
#   packageOverrides = pkgs: with pkgs; {
#     focusPackages = pkgs.callPackage ~/.emacs.d/focus.nix pkgs;
#   };
# }

pkgs: with pkgs; {
  focusPackages = pkgs.buildEnv {
    name = "emacs-packages-config";
    paths = [
      emacsPackages.cask
      emacsPackages.groovy-mode
      emacsPackages.docker-tramp
      # lsp mode
      emacsPackages.lsp-mode
      emacsPackages.lsp-ui
      # lsp ext
      emacsPackages.lsp-treemacs
      emacsPackages.lsp-docker
      emacsPackages.lsp-origami
      # lsp langs
      emacsPackages.lsp-metals
      emacsPackages.lsp-haskell
      emacsPackages.lsp-java
      emacsPackages.dap-mode
    ];
  };
}
