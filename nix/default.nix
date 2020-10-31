{ pkgs, lib, ocamlPackages, doCheck ? false }:

with ocamlPackages;

rec {
  graphql-sdl-parser = buildDunePackage {
    pname = "graphql-sdl-parser";
    version = "0.0.1-dev";

    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "lib" "lib_test" ];
      files = [ "dune-project" "piaf.opam" ];
    };

    useDune2 = true;

    propagatedBuildInputs = [
      menhir
      graphql
      yojson
      easy-format

      reason
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };
}

