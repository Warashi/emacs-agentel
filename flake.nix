{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    elisp-helpers.url = "github:emacs-twist/elisp-helpers";
    twist.url = "github:emacs-twist/twist.nix";
    twist.inputs.elisp-helpers.follows = "elisp-helpers";
    registries.url = "github:emacs-twist/registries";
  };

  outputs =
    inputs:
    let
      inherit (inputs.nixpkgs) lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: lib.genAttrs systems (system: f (import inputs.nixpkgs { inherit system; }));

      fromElisp = (inputs.elisp-helpers.lib.makeLib { inherit lib; }).fromElisp.fromElisp;
      packageRequires = lib.pipe ./agentel.el [
        builtins.readFile
        (lib.splitString "\n")
        (lib.findFirst (line: lib.hasPrefix ";; Package-Requires:" line) (
          throw "agentel.el has no Package-Requires header"
        ))
        (lib.removePrefix ";; Package-Requires:")
        fromElisp
        builtins.head
        (map builtins.head)
        (lib.remove "emacs")
      ];

      # agentel itself is loaded from the working tree so that editing it does
      # not rebuild the environment.
      emacsFor =
        pkgs:
        inputs.twist.lib.makeEnv {
          inherit pkgs;
          emacsPackage = pkgs.emacs-nox;
          lockDir = ./lock;
          registries = inputs.registries.lib.registries;
          initFiles = [ ];
          extraPackages = packageRequires;
          persistMetadata = true;
        };
    in
    {
      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
      apps = forAllSystems (pkgs: (emacsFor pkgs).makeApps { lockDirName = "lock"; });
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            (emacsFor pkgs)
            pkgs.git
            pkgs.tmux
          ];
        };
      });
    };
}
