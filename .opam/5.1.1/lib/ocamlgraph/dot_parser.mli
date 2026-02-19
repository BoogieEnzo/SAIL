type token =
  | ID of (
# 38 "src/dot_parser.mly"
        Dot_ast.id
# 6 "src/dot_parser.mli"
)
  | COLON
  | COMMA
  | EQUAL
  | SEMICOLON
  | EDGEOP
  | STRICT
  | GRAPH
  | DIGRAPH
  | LBRA
  | RBRA
  | LSQ
  | RSQ
  | NODE
  | EDGE
  | SUBGRAPH
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Dot_ast.file
