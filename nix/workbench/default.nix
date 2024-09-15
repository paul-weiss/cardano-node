{ pkgs
, lib
, cardanoNodePackages
, cardanoNodeProject
}:

with lib;

let
  # recover CHaP location from cardano's project
  chap = cardanoNodeProject.args.inputMap."https://chap.intersectmbo.org/";
  # build plan as computed by nix
  nixPlanJson = cardanoNodeProject.plan-nix.json;

  workbench' = tools:
    pkgs.stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = with pkgs; [ makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb"                \
          --argv0 wb                             \
          --prefix PATH ":" ${makeBinPath tools} \
          --set WB_CHAP_PATH ${chap}             \
          --set WB_NIX_PLAN ${nixPlanJson}
      '';

      installPhase = ''
        mkdir -p                                                    $out/bin
        cp    -a wb ede profile                                     $out/bin
        for dir in . analyse backend genesis topology
        do cp    -a $dir/*                                          $out/bin/$dir
        done
      '';

      dontStrip = true;
    };

  workbench = with cardanoNodePackages; with pkgs; workbench' (
    [ git graphviz
      jq
      moreutils
      procps
      cardano-cli
      cardano-profile
      cardano-topology
    ] ++ lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) db-analyser
      ++ [ locli ]
    );

  runWorkbench =
    name: command:
    pkgs.runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    pkgs.runCommand name {} ''
      args=(${args})
      ${pkgs.jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  profile-names-json =
    runWorkbench "profile-names.json" "profiles list";

  profile-names =
    __fromJSON (__readFile profile-names-json);

in pkgs.lib.fix (self: {
  inherit cardanoNodePackages;
  inherit workbench' workbench runWorkbench;
  inherit runJq;

  inherit profile-names-json profile-names;

  # Return a backend attr with a `materialise-profile` function.
  # backendName -> stateDir -> basePort -> useCabalRun -> backend
  backend =
    let backendRegistry = {
        nomadcloud      = params:
          import ./backend/nomad/cloud.nix  params;
        nomadexec       = params:
          import ./backend/nomad/exec.nix   params;
        supervisor      = params:
          import ./backend/supervisor.nix   params;
        };
  in { backendName
     , stateDir
     , basePort
     , useCabalRun
     }:
      # The `useCabalRun` flag is set in the backend to allow the backend to
      # override its value. The runner uses the value of `useCabalRun` from
      # the backend to prevent a runner using a different value.
      (backendRegistry."${backendName}")
        { inherit pkgs lib stateDir basePort useCabalRun; }
  ;

  # Return a profile attr with a `materialise-profile` function.
  # profileName -> profiling -> profile
  profile =
    { profileName
    , profiling
    }:
    (import ./profile/profile.nix
      { inherit pkgs lib;
        workbenchNix = self;
        inherit profileName profiling;
      }
    )
  ;

  # A conveniently-parametrisable workbench preset.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # The general idea is:
  # 1. profileName -> profiling -> profile
  # 2. backendName -> stateDir -> basePort -> useCabalRun -> backend
  # 3. profile -> backend -> batchName -> runner
  runner =
    { stateDir
    , batchName
    , profileName
    , backendName
    , basePort
    , useCabalRun
    , workbenchDevMode
    , workbenchStartArgs
    , profiling
    , cardano-node-rev
    }:
    let
        # Only a name needed to create a profile attrset.
        profile = self.profile
                    { inherit profileName profiling; }
        ;
        # The `useCabalRun` flag is set in the backend to allow the backend to
        # override its value. The runner uses the value of `useCabalRun` from
        # the backend to prevent a runner using a different value.
        backend = self.backend
                    { inherit backendName stateDir basePort useCabalRun; }
        ;
    in import ./backend/runner.nix
      {
          inherit pkgs lib;
          inherit profile backend;
          inherit batchName;
          inherit cardano-node-rev;
          inherit workbench; # The derivation.
          inherit cardanoNodePackages;
          inherit workbenchDevMode workbenchStartArgs;
      };
})
