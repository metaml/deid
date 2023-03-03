{
  description = "deid";

  inputs = {
    nixpkgs      = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    flake-utils  = { url = "github:numtide/flake-utils"; };
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };

  outputs = { self, nixpkgs, flake-compat, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pname = "deid";
        ghc-version = "ghc926";
        pkgs = nixpkgs.legacyPackages.${system};
        haskell-pkgs = pkgs.haskell.packages.${ghc-version}; # "make update" first sometimes helps
        uname = nixpkgs.lib.lists.last (nixpkgs.lib.strings.split "-" "${system}");

        deid = pkgs.runCommand
          "deid"
          { preferLocalBuild = true;  buildInputs = [ pname ]; }
          "";

        revision = "${self.lastModifiedDate}-${self.shortRev or "dirty"}";
      in rec {
        # nix build
        packages.${system}.default = pkgs.stdenvNoCC.mkDerivation {
          name = "${pname}";
          src = self;
          buildInputs = [
            haskell-pkgs.cabal-install
            haskell-pkgs.ghc
            haskell-pkgs.zlib
            pkgs.cacert
            pkgs.git
            pkgs.gmp
            pkgs.google-cloud-sdk
            pkgs.libcxx
            pkgs.llvm
            pkgs.llvmPackages.clang
            pkgs.pcre
            pkgs.pkg-config
            pkgs.zlib.dev
          ] ++ (
            if "darwin" == "${uname}"
            then [pkgs.${uname}.apple_sdk.frameworks.Cocoa]
            else []
          );
          buildPhase = "
            export CABAL_DIR=$out
            cabal update --verbose
            cabal build --verbose
          ";
          installPhase = "";
        };

        defaultPackage = packages.${system}.default;

        # nix develop
        devShell = pkgs.mkShell {
          buildInputs = with haskell-pkgs; [
            cabal-install
            fsnotify
            fswatcher
            ghc
            haskell-language-server
            hlint
            pkgs.cacert
            pkgs.git
            pkgs.gmp
            pkgs.google-cloud-sdk
            pkgs.libcxx
            pkgs.nix
            pkgs.pcre
            pkgs.pkg-config
            pkgs.sourceHighlight
            pkgs.zlib.dev
          ];
          shellHook = ''
            export LANG=en_US.UTF-8
            export GOOGLE_PROJECT=lpgprj-gss-p-ctrlog-gl-01
            export GOOGLE_REGION=us-east1
            export GOOGLE_ZONE=us-east1-c
            export PS1="nix|-$PS1"
          '';
        };
      }
    );
}
