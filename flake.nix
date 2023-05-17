# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    telegram-bot-monadic.url = "github:balsoft/telegram-bot-monadic";
  };

  outputs = { self, nixpkgs, flake-utils, telegram-bot-monadic }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages =
          pkgs.haskellPackages.extend telegram-bot-monadic.overlays.default;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "parki-ar-minda-bot";
      in {
        packages = {
          default = self.packages.${system}.${packageName};
          ${packageName} = haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };
          "${packageName}-static" = (pkgs.pkgsStatic.haskellPackages.extend
            telegram-bot-monadic.overlays.default).callCabal2nix packageName
            self { };
        };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = [ self.packages.${system}.default.env ];
        };
      });
}
