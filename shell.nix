let
  pkgs = import <nixpkgs> { };

  ghc = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hnix = self.callPackage (import ./hnix.nix pkgs.fetchFromGitHub) {};
    };
  };

  ghcPackages = ghc.ghcWithHoogle (p: with p; [
    ipprint
    ghc-mod
    hdevtools
    stylish-haskell
    cabal-install
    pretty-show
    cabal2nix
    ghcid

    derive
    lens
    lens-aeson
    uniplate
    hnix
    parsec
    split
    aeson
    text
    vector
    hashmap
    unordered-containers
    haskell-generate
    haskell-src-exts
    haskell-src-exts-qq
    dynamic-mvector

    hspec
  ]);

in

pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    ghcPackages

    python27Full
    pythonPackages.pycapnp
    capnproto
  ];
  shellHook = ''
    export NIX_GHC="${ghcPackages}/bin/ghc"
    export NIX_GHCPKG="${ghcPackages}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghcPackages}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
} ""
