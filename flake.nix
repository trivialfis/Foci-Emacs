{
  description = "Foci";

  inputs = {emacs-overlay.url = "github:nix-community/emacs-overlay";};
  outputs = { self, nixpkgs, emacs-overlay }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in {
          default = import ./foci.nix { pkgs = pkgs; };
        }
      );
    };
}
