# Prefix of the current opam switch
if ( ! ${?OPAM_SWITCH_PREFIX} ) setenv OPAM_SWITCH_PREFIX ""
setenv OPAM_SWITCH_PREFIX '/home/fengde/SAIL/.opam/5.1.1'
# Updated by package ocaml-base-compiler
if ( ! ${?CAML_LD_LIBRARY_PATH} ) setenv CAML_LD_LIBRARY_PATH ""
setenv CAML_LD_LIBRARY_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/stublibs'
# Updated by package ocaml
if ( ! ${?OCAMLTOP_INCLUDE_PATH} ) setenv OCAMLTOP_INCLUDE_PATH ""
setenv OCAMLTOP_INCLUDE_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/toplevel':"$OCAMLTOP_INCLUDE_PATH"
# Updated by package ocaml
if ( ! ${?CAML_LD_LIBRARY_PATH} ) setenv CAML_LD_LIBRARY_PATH ""
setenv CAML_LD_LIBRARY_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/ocaml/stublibs:/home/fengde/SAIL/.opam/5.1.1/lib/ocaml'
# Updated by package ocaml
if ( ! ${?CAML_LD_LIBRARY_PATH} ) setenv CAML_LD_LIBRARY_PATH ""
setenv CAML_LD_LIBRARY_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/stublibs':"$CAML_LD_LIBRARY_PATH"
# Updated by package ocaml
if ( ! ${?OCAML_TOPLEVEL_PATH} ) setenv OCAML_TOPLEVEL_PATH ""
setenv OCAML_TOPLEVEL_PATH '/home/fengde/SAIL/.opam/5.1.1/lib/toplevel'
# Current opam switch man dir
if ( ! ${?MANPATH} ) setenv MANPATH ""
setenv MANPATH "$MANPATH":'/home/fengde/SAIL/.opam/5.1.1/man'
# Binary dir for opam switch 5.1.1
if ( ! ${?PATH} ) setenv PATH ""
setenv PATH '/home/fengde/SAIL/.opam/5.1.1/bin':"$PATH"
