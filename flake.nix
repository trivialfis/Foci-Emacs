{
  description = "Foci";

  inputs = {emacs-overlay.url = "github:nix-community/emacs-overlay";};
  outputs = { self, nixpkgs, emacs-overlay }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlays.default ];
        }
      );
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in {
          default = import ./foci.nix { pkgs = pkgs; };
        }
      );
      apps = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          emacs = self.packages.${system}.default;
        in {
          default = {
            type = "app";
            program = "${pkgs.writeShellScript "foci-compile" ''
              EMACS_DIR="''${1:-$HOME/.emacs.d}"
              exec ${pkgs.gnumake}/bin/make -C "$EMACS_DIR" recompile EMACS=${emacs}/bin/emacs
            ''}";
          };
        }
      );
    };
}
