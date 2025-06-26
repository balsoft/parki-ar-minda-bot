# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    telegram-bot-monadic.url = "github:balsoft/telegram-bot-monadic";
    telegram-bot-simple.url = "github:balsoft/telegram-bot-simple/fix-ChatFullInfo";
    telegram-bot-simple.flake = false;
    iCalendar.url = "github:chrra/iCalendar";
    iCalendar.flake = false;
    persistent = {
      url = "github:balsoft/persistent?ref=balsoft/sqlite-fix-migrations";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      telegram-bot-monadic,
      telegram-bot-simple,
      iCalendar,
      persistent,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        jailbreakUnbreak =
          pkg:
          pkgs.haskell.lib.doJailbreak (
            pkg.overrideAttrs (_: {
              meta = { };
            })
          );
        overlay = pkgs.lib.composeManyExtensions [
          telegram-bot-monadic.overlays.default
          (final: prev: {
            telegram-bot-api = prev.telegram-bot-api.overrideAttrs (_: {
              src = "${telegram-bot-simple}/telegram-bot-api";
            });
            telegram-bot-simple = prev.telegram-bot-simple.overrideAttrs (_: {
              src = "${telegram-bot-simple}/telegram-bot-simple";
            });
            iCalendar = jailbreakUnbreak (prev.callCabal2nix "iCalendar" iCalendar { });

            persistent-sqlite = prev.persistent-sqlite.overrideAttrs (_: {
              src = persistent + /persistent-sqlite;
            });
          })
        ];

        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.extend overlay;
        haskellPackagesStatic = pkgs.pkgsStatic.haskellPackages.extend overlay;

        packageName = "parki-ar-minda-bot";
      in
      {
        packages = {
          default = self.packages.${system}.${packageName};
          ${packageName} = haskellPackages.callCabal2nix packageName self { };
          "${packageName}-static" = haskellPackagesStatic.callCabal2nix packageName self { };
        };

        defaultPackage = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            # cabal-install
            sqlite-interactive
          ];
          inputsFrom = [ self.packages.${system}.default.env ];
        };
      }
    );
}
