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
        ghc-version = "ghc928";
        pkgs = nixpkgs.legacyPackages.${system};
        hkgs = pkgs.haskell.packages.${ghc-version}; # "make update" first sometimes helps
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
          buildInputs = with pkgs; [
            cacert
            git
            gmp
            google-cloud-sdk
            hkgs.cabal-install
            hkgs.ghc
            hkgs.zlib
            libcxx
            llvm
            llvmPackages.clang
            pcre
            pkg-config
            zlib.dev
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
          buildInputs = with pkgs; [
            cacert
            git
            gmp
            google-cloud-sdk
            hkgs.cabal-install
            hkgs.ghc
            hkgs.haskell-language-server
            hkgs.hlint
            pcre.dev
            sourceHighlight
            zlib.dev
          ];
          shellHook = ''
            export LANG=en_US.UTF-8
            export GOOGLE_PROJECT=lpgprj-gss-p-ctrlog-gl-01
            export GOOGLE_REGION=us-east1
            export GOOGLE_ZONE=us-east1-c
            export PS1="babel|$PS1"
          '';
        };
      }
    );
}
