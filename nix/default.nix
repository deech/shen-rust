{ rustRelease ? { channel = "nightly"; date = "2019-07-15"; } }:

let
  # NIXPKGS RELEASE-19.03 (STABLE) BRANCH @ 2019-07-09
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/28e64db237dbe1e0d430cb61e7372223b49635b9.tar.gz";
    sha256 = "0hwy0b1axikzx7xlcqf7p7698rhpw3x0hlsm2hk0j8iv6ampdgda";
  };
in

import nixpkgs {
  overlays = [
    # MOZILLA (RUST) OVERLAY (MASTER BRANCH @ 2019-07-09)
    (import
      (builtins.fetchTarball
        https://github.com/mozilla/nixpkgs-mozilla/archive/200cf0640fd8fdff0e1a342db98c9e31e6f13cd7.tar.gz))
  ];

  config = {
    packageOverrides = super:
      let self = super.pkgs;
          # TRANSFORM NIXPKGS TARGET -> RUSTC TARGET
          target = builtins.replaceStrings
            ["armv7l"] ["armv7"]
            self.targetPlatform.config;
      in {
        rust =
          self.rustChannelOfTargets
            rustRelease.channel
            rustRelease.date
            [ target ];
        rustc = self.rust; # (A METAPACKAGE THAT INCLUDES 'rustc')
        cargo = self.rust; # (A METAPACKAGE THAT INCLUDES 'cargo')
        cratesIO = self.callPackage ./crates-io.nix {};
        shen_rust = (self.callPackage ../Cargo.nix {}).shen_rust {};
    };
  };
}
