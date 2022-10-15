{
  description = "A VTE-based terminal emulator configurable in Haskell";

  # Nixpkgs / NixOS version to use.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let

      # Generate a user-friendly version numer.
      version = builtins.substring 0 8 self.lastModifiedDate;

      # System types to support.
      #
      # TODO: Since callCabal2nix uses IFD, adding multiple systems to
      # supportedSystems doesn't currently work with flakes.  Commands like
      # `nix flake check` and `nix flake show` don't work.
      #
      # https://github.com/NixOS/nix/issues/4265
      #
      # Termonad is likely to also work aarch64-linux, but you'll have to edit
      # this line to enable it.
      supportedSystems = [ "x86_64-linux" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });

    in

    {

      # A Nixpkgs overlay that defines Termonad.
      overlay = final: prev:
        let
          overlays = import ./.nix-helpers/overlays.nix;
        in
        prev.lib.composeManyExtensions overlays final prev;

      # Provide some binary packages for selected system types.
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in {
          termonad = pkgs.termonad-with-packages;
        }
      );

      # The default package for 'nix build'. This makes sense if the
      # flake provides only one package or there is a clear "main"
      # package.
      defaultPackage = forAllSystems (system: self.packages.${system}.termonad);

      devShell = forAllSystems (system: nixpkgsFor.${system}.termonadShell);

      defaultApp = forAllSystems (system: self.apps.${system}.termonad);

      apps = forAllSystems (system: {
        termonad = {
          type = "app";
          program = "${self.packages.${system}.termonad}/bin/termonad";
        };
      });

      # Tests run by 'nix flake check' and by Hydra.
      # checks = forAllSystems
      #   (system:
      #     with nixpkgsFor.${system};
      #     {
      #       inherit (self.packages.${system}) hello;

      #       # Additional tests, if applicable.
      #       test = stdenv.mkDerivation {
      #         name = "hello-test-${version}";

      #         buildInputs = [ hello ];

      #         unpackPhase = "true";

      #         buildPhase = ''
      #           echo 'running some integration tests'
      #           [[ $(hello) = 'Hello Nixers!' ]]
      #         '';

      #         installPhase = "mkdir -p $out";
      #       };
      #     }
      #   );

    };
}
