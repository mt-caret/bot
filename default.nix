let
  pkgs = import (import ./fetch-nixpkgs.nix) {};
  bot = pkgs.haskell.packages.ghc802.callPackage ./bot.nix {};
  gpu = false;
in
  pkgs.haskell.lib.overrideCabal bot ( oldDerivation: {
    executableSystemDepends = [
      pkgs.wget
      (pkgs.python.withPackages (ps: [
        (if gpu then (ps.tensorflow.override {
          cudaSupport = true;
          cudnn = pkgs.stdenv.mkDerivation rec {
            version = "7.0.4";
            name = "cudnn-${version}";
            src = pkgs.requireFile rec {
              name = "cudnn-9.0-linux-x64-v7.tgz";
              message = "yo";
              sha256 = "0kxwnl4zfjdib01a077jqnyx3khwrf6kjm67n3d1d1i9gh2s4gcn";
            };
            phases = "unpackPhase installPhase fixupPhase";
            propagatedBuildInputs = [ pkgs.cudatoolkit ];
            installPhase = ''
              mkdir -p $out
              cp -a include $out/include
              cp -a lib64 $out/lib64
            '';
          };
        }) else ps.tensorflow)
        ps.matplotlib
        ps.scikitimage
        ps.numpy
        (ps.buildPythonPackage rec {
          name = "${pname}-${version}";
          pname = "lime";
          version = "0.1.1.25";
          src = ps.fetchPypi {
            inherit pname version;
            sha256 = "0p2ym4qwaidd9fgy0hgynf63qlscj3f9abj467spfg48ii9qp9jp";
          };
          propagatedBuildInputs = [ ps.scikitlearn ps.scikitimage ps.dask ];
        })
      ]))
    ];
  })
