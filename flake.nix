{
  description = "emacs configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # emacs-ng = {
    #   url = "github:emacs-ng/emacs-ng";
    #   inputs.emacs-overlay.follows = "emacs-overlay";
    # }; # emacs with some wacky things like web rendering. also rust
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    emacs-overlay,
    # emacs-ng,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          system = system;
          overlays = [
            emacs-overlay.overlays.default
            #emacs-ng.overlays.default
          ]; # gets emacs and package overlays
        };

        propogatedBuildInputs = [
          #runtime deps
          pkgs.tree-sitter-grammars.tree-sitter-elisp
          (pkgs.python3.withPackages (p:
            # I think this works for lsp-bridge
              with p; [
                epc
                orjson
                sexpdata
                six
                setuptools
                paramiko
                rapidfuzz
              ]))
        ];
        # ] ++ with pkgs.python311Packages [
        #       epc
        #       orjson
        #       sexpdata
        #       six
        #       setuptools
        #       paramiko
        #       rapidfuzz
        #     ];
      in {
        packages."emacs" = pkgs.stdenv.mkDerivation {
          name = "emacs";
          src = self.packages.${system}.emacsUnwrapped;

          buildPhase = ''
            mkdir $out
            cp -r * $out/
          '';
          buildInputs = propogatedBuildInputs;
        };

        packages.default = self.packages.${system}."emacs";
        devShells.default = pkgs.mkShell {
          packages = propogatedBuildInputs;
        };
        packages.emacsUnwrapped = pkgs.emacsWithPackagesFromUsePackage {
          # Your Emacs config file. Org mode babel files are also
          # supported.
          # NB: Config files cannot contain unicode characters, since
          #     they're being parsed in nix, which lacks unicode
          #     support.
          # config = ./emacs.org;
          config = ./emacs.el;

          # Whether to include your config as a default init file.
          # If being bool, the value of config is used.
          # Its value can also be a derivation like this if you want to do some
          # substitution:
          #   defaultInitFile = pkgs.substituteAll {
          #     name = "default.el";
          #     src = ./emacs.el;
          #     inherit (config.xdg) configHome dataHome;
          #   };
          defaultInitFile = true;

          # Package is optional, defaults to pkgs.emacs
          # package = pkgs.emacsngWRPgtk; #emacs-ng build
          package = pkgs.emacs-pgtk; # bleeding edge pgtk
          # package = pkgs.emacs29-pgtk; # stable latest release pgtk

          # By default emacsWithPackagesFromUsePackage will only pull in
          # packages with `:ensure`, `:ensure t` or `:ensure <package name>`.
          # Setting `alwaysEnsure` to `true` emulates `use-package-always-ensure`
          # and pulls in all use-package references not explicitly disabled via
          # `:ensure nil` or `:disabled`.
          # Note that this is NOT recommended unless you've actually set
          # `use-package-always-ensure` to `t` in your config.
          alwaysEnsure = true;

          # For Org mode babel files, by default only code blocks with
          # `:tangle yes` are considered. Setting `alwaysTangle` to `true`
          # will include all code blocks missing the `:tangle` argument,
          # defaulting it to `yes`.
          # Note that this is NOT recommended unless you have something like
          # `#+PROPERTY: header-args:emacs-lisp :tangle yes` in your config,
          # which defaults `:tangle` to `yes`.
          # alwaysTangle = true;

          # Optionally provide extra packages not in the configuration file.
          extraEmacsPackages = epkgs: [
            #epkgs => pkgs.emacsPackages namespace in nix
            epkgs.vterm # this supposedly neesd to go here. Maybe due to external library issues?
            epkgs.treesit-grammars.with-all-grammars
            epkgs.use-package
	    epkgs.jinx # needs libenchant
	    epkgs.lsp-bridge # just in case
          ];
        };
      }
    );
}
