{ sources ? import ./sources.nix }:

let compiler = "ghc902";

    # Inspired by https://github.com/dhall-lang/dhall-haskell/blob/master/nix/shared.nix
    haskellStaticOverlay = self: super: {
      haskellPackages = self.haskell.packages."${compiler}";

      fixedCabal = self.pkgsMusl.haskell.packages."${compiler}".Cabal_3_6_3_0;

      haskell = super.haskell // {
        lib = super.haskell.lib // {
          useFixedCabal = drv:
            (self.haskell.lib.overrideCabal drv (old: {
                setupHaskellDepends = (old.setupHaskellDepends or []) ++ [ self.fixedCabal ];
              }
            )).overrideAttrs (old: {
                preCompileBuildDriver = (old.preCompileBuildDriver or "") + ''
                  cabalPackageId=$(basename --suffix=.conf ${self.fixedCabal}/lib/ghc-*/package.conf.d/*.conf)
                  setupCompileFlags="$setupCompileFlags -package-id $cabalPackageId"
                '';
              }
            );

          mkStaticExe = drv:
            self.haskell.lib.appendConfigureFlags
              (self.haskell.lib.justStaticExecutables
                (self.haskell.lib.useFixedCabal (self.haskell.lib.dontCheck drv))
              )
              [ "--enable-executable-static"
                "--extra-lib-dirs=${self.pkgsMusl.ncurses.override { enableStatic = true; }}/lib"
                "--extra-lib-dirs=${self.pkgsMusl.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${self.pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              ];
        };
      };
    };

    overlay = self: super: {
      inherit (import sources."gitignore.nix" { inherit (self) lib; }) gitignoreSource;
    };
in
import sources.nixpkgs { overlays = [haskellStaticOverlay overlay] ; config = {}; }
