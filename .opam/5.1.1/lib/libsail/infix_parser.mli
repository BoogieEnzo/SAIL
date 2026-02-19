
(* The type of tokens. *)

type token = 
  | Typ of (Parse_ast.atyp)
  | TwoCaret
  | Star
  | Plus
  | Op9r of (Parse_ast.id)
  | Op9l of (Parse_ast.id)
  | Op9 of (Parse_ast.id)
  | Op8r of (Parse_ast.id)
  | Op8l of (Parse_ast.id)
  | Op8 of (Parse_ast.id)
  | Op7r of (Parse_ast.id)
  | Op7l of (Parse_ast.id)
  | Op7 of (Parse_ast.id)
  | Op6r of (Parse_ast.id)
  | Op6l of (Parse_ast.id)
  | Op6 of (Parse_ast.id)
  | Op5r of (Parse_ast.id)
  | Op5l of (Parse_ast.id)
  | Op5 of (Parse_ast.id)
  | Op4r of (Parse_ast.id)
  | Op4l of (Parse_ast.id)
  | Op4 of (Parse_ast.id)
  | Op3r of (Parse_ast.id)
  | Op3l of (Parse_ast.id)
  | Op3 of (Parse_ast.id)
  | Op2r of (Parse_ast.id)
  | Op2l of (Parse_ast.id)
  | Op2 of (Parse_ast.id)
  | Op1r of (Parse_ast.id)
  | Op1l of (Parse_ast.id)
  | Op1 of (Parse_ast.id)
  | Op0r of (Parse_ast.id)
  | Op0l of (Parse_ast.id)
  | Op0 of (Parse_ast.id)
  | Minus
  | LtEq
  | Lt
  | In
  | GtEq
  | Gt
  | Exp of (Parse_ast.exp)
  | Eof
  | ColonColon
  | At

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val typ_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.atyp)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.exp)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val typ_eof: Lexing.position -> (Parse_ast.atyp) MenhirInterpreter.checkpoint
  
  val exp_eof: Lexing.position -> (Parse_ast.exp) MenhirInterpreter.checkpoint
  
end
