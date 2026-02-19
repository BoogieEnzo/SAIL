
(* The type of tokens. *)

type token = 
  | Variable
  | Var of (string)
  | True
  | Then
  | Test
  | String of (string)
  | Slash
  | Semi
  | Rsquare
  | Rparen
  | Requires
  | Rcurly
  | LtEq
  | Lt
  | Lsquare
  | Lparen
  | Lcurly
  | If
  | IdLcurly of (string * Lexing.position)
  | Id of (string)
  | GtEq
  | Gt
  | Files
  | FileId of (string * string)
  | False
  | ExclEq
  | EqEq
  | Eq
  | Eof
  | Else
  | DotDot
  | Directory
  | Comma
  | Before
  | After

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Project.def Project.spanned list)
