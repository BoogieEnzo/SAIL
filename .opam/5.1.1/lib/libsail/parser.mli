
(* The type of tokens. *)

type token = 
  | With
  | While
  | When
  | Var
  | Val
  | Until
  | Unit of (string)
  | Union
  | Under
  | Undefined
  | Typedef
  | TyVar of (string)
  | TwoCaret
  | Try
  | True
  | To
  | Throw
  | Then
  | TerminationMeasure
  | TYPE
  | StructuredPragma of (string)
  | Struct
  | String of (string)
  | Star
  | Sizeof
  | Semi
  | Scattered
  | RsquareBar
  | Rsquare
  | Rparen
  | Return
  | Repeat
  | Register
  | Ref
  | Real of (string)
  | RcurlyBar
  | Rcurly
  | Pure
  | Private
  | Pragma of (string * string)
  | Overload
  | Outcome
  | OpId of (string)
  | Op
  | ORDER
  | Num of (Nat_big_num.num)
  | Newtype
  | NAT
  | Mutual
  | MultilineString of (string list)
  | Monadic
  | MinusGt
  | Minus
  | Match
  | Mapping
  | LsquareBar
  | Lsquare
  | Lparen
  | Let_
  | LcurlyBar
  | Lcurly
  | InternalReturn
  | InternalPLet
  | InternalAssume
  | Instantiation
  | Inc
  | In
  | Impure
  | Impl
  | If_
  | Id of (string)
  | INT
  | Hex of (string)
  | Function_
  | From
  | Forwards
  | Foreach
  | Forall
  | Fixity of (Parse_ast.fixity_token)
  | False
  | Exit
  | EqGt of (string)
  | Eq of (string)
  | Eof
  | Enum
  | End
  | Else
  | Effect
  | Downto
  | DotDot
  | Dot
  | DocLine of (string)
  | DocBlock of (string)
  | Do
  | Default
  | Dec
  | Constraint
  | Constant
  | Configuration
  | Config
  | Comma
  | ColonColon
  | Colon of (string)
  | Clause
  | Catch
  | Cast
  | Caret
  | By
  | Bitzero
  | Bitone
  | Bitfield
  | Bin of (string)
  | Bidir
  | Bar
  | Backwards
  | BOOL
  | Attribute of (string)
  | At
  | Assert
  | As
  | And

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val typschm_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.typschm)

val typ_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.atyp)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.def list)

val exp_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.exp)

val def_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.def)

val attribute_data_eof: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parse_ast.Attribute_data.attribute_data)
