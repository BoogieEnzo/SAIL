# Prefix of the current opam switch
set -gx OPAM_SWITCH_PREFIX '/home/fengde/SAIL/.opam/5.1.1';
# Updated by package ocaml-base-compiler
set -gx CAML_LD_LIBRARY_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/stublibs';
# Updated by package ocaml
set -gx OCAMLTOP_INCLUDE_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/toplevel':"$OCAMLTOP_INCLUDE_PATH";
# Updated by package ocaml
set -gx CAML_LD_LIBRARY_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/ocaml/stublibs:/home/fengde/SAIL/.opam/5.1.1/lib/ocaml';
# Updated by package ocaml
set -gx CAML_LD_LIBRARY_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/stublibs':"$CAML_LD_LIBRARY_PATH";
# Updated by package ocaml
set -gx OCAML_TOPLEVEL_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/toplevel';
# Current opam switch man dir
if [ (count $MANPATH) -gt 0 ]; set -gx MANPATH $MANPATH '/home/fengde/SAIL/.opam/5.1.1/man'; end;
# Binary dir for opam switch 5.1.1
set -gx PATH '/home/fengde/SAIL/.opam/5.1.1/bin' $PATH;
