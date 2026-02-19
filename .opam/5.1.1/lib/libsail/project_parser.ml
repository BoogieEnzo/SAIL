
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | Variable
    | Var of (
# 66 "src/lib/project_parser.mly"
       (string)
# 16 "src/lib/project_parser.ml"
  )
    | True
    | Then
    | Test
    | String of (
# 65 "src/lib/project_parser.mly"
       (string)
# 24 "src/lib/project_parser.ml"
  )
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
    | IdLcurly of (
# 64 "src/lib/project_parser.mly"
       (string * Lexing.position)
# 41 "src/lib/project_parser.ml"
  )
    | Id of (
# 63 "src/lib/project_parser.mly"
       (string)
# 46 "src/lib/project_parser.ml"
  )
    | GtEq
    | Gt
    | Files
    | FileId of (
# 62 "src/lib/project_parser.mly"
       (string * string)
# 54 "src/lib/project_parser.ml"
  )
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
  
end

include MenhirBasics

# 47 "src/lib/project_parser.mly"
  

[@@@coverage exclude_file]

open Project

let span x s e = (x, (s, e))


# 82 "src/lib/project_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_file) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: file. *)

  | MenhirState03 : (('s, _menhir_box_file) _menhir_cell1_Variable _menhir_cell0_Id, _menhir_box_file) _menhir_state
    (** State 03.
        Stack shape : Variable Id.
        Start symbol: file. *)

  | MenhirState07 : (('s, _menhir_box_file) _menhir_cell1_Lsquare, _menhir_box_file) _menhir_state
    (** State 07.
        Stack shape : Lsquare.
        Start symbol: file. *)

  | MenhirState09 : (('s, _menhir_box_file) _menhir_cell1_Lparen, _menhir_box_file) _menhir_state
    (** State 09.
        Stack shape : Lparen.
        Start symbol: file. *)

  | MenhirState10 : (('s, _menhir_box_file) _menhir_cell1_If, _menhir_box_file) _menhir_state
    (** State 10.
        Stack shape : If.
        Start symbol: file. *)

  | MenhirState12 : (('s, _menhir_box_file) _menhir_cell1_Id _menhir_cell0_Lparen, _menhir_box_file) _menhir_state
    (** State 12.
        Stack shape : Id Lparen.
        Start symbol: file. *)

  | MenhirState18 : (('s, _menhir_box_file) _menhir_cell1_slash_exp, _menhir_box_file) _menhir_state
    (** State 18.
        Stack shape : slash_exp.
        Start symbol: file. *)

  | MenhirState21 : (('s, _menhir_box_file) _menhir_cell1_atomic_exp, _menhir_box_file) _menhir_state
    (** State 21.
        Stack shape : atomic_exp.
        Start symbol: file. *)

  | MenhirState23 : (('s, _menhir_box_file) _menhir_cell1_slash_exp, _menhir_box_file) _menhir_state
    (** State 23.
        Stack shape : slash_exp.
        Start symbol: file. *)

  | MenhirState25 : (('s, _menhir_box_file) _menhir_cell1_slash_exp, _menhir_box_file) _menhir_state
    (** State 25.
        Stack shape : slash_exp.
        Start symbol: file. *)

  | MenhirState27 : (('s, _menhir_box_file) _menhir_cell1_slash_exp, _menhir_box_file) _menhir_state
    (** State 27.
        Stack shape : slash_exp.
        Start symbol: file. *)

  | MenhirState29 : (('s, _menhir_box_file) _menhir_cell1_slash_exp, _menhir_box_file) _menhir_state
    (** State 29.
        Stack shape : slash_exp.
        Start symbol: file. *)

  | MenhirState31 : (('s, _menhir_box_file) _menhir_cell1_slash_exp, _menhir_box_file) _menhir_state
    (** State 31.
        Stack shape : slash_exp.
        Start symbol: file. *)

  | MenhirState36 : (('s, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_state
    (** State 36.
        Stack shape : exp.
        Start symbol: file. *)

  | MenhirState37 : ((('s, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_cell1_Comma, _menhir_box_file) _menhir_state
    (** State 37.
        Stack shape : exp Comma.
        Start symbol: file. *)

  | MenhirState39 : (('s, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_state
    (** State 39.
        Stack shape : exp.
        Start symbol: file. *)

  | MenhirState40 : ((('s, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_cell1_Comma, _menhir_box_file) _menhir_state
    (** State 40.
        Stack shape : exp Comma.
        Start symbol: file. *)

  | MenhirState45 : ((('s, _menhir_box_file) _menhir_cell1_If, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_state
    (** State 45.
        Stack shape : If exp.
        Start symbol: file. *)

  | MenhirState47 : (((('s, _menhir_box_file) _menhir_cell1_If, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_cell1_op_exp, _menhir_box_file) _menhir_state
    (** State 47.
        Stack shape : If exp op_exp.
        Start symbol: file. *)

  | MenhirState54 : (('s, _menhir_box_file) _menhir_cell1_Test, _menhir_box_file) _menhir_state
    (** State 54.
        Stack shape : Test.
        Start symbol: file. *)

  | MenhirState55 : (('s, _menhir_box_file) _menhir_cell1_Id, _menhir_box_file) _menhir_state
    (** State 55.
        Stack shape : Id.
        Start symbol: file. *)

  | MenhirState58 : (('s, _menhir_box_file) _menhir_cell1_IdLcurly, _menhir_box_file) _menhir_state
    (** State 58.
        Stack shape : IdLcurly.
        Start symbol: file. *)

  | MenhirState59 : (('s, _menhir_box_file) _menhir_cell1_Requires, _menhir_box_file) _menhir_state
    (** State 59.
        Stack shape : Requires.
        Start symbol: file. *)

  | MenhirState60 : (('s, _menhir_box_file) _menhir_cell1_Lcurly, _menhir_box_file) _menhir_state
    (** State 60.
        Stack shape : Lcurly.
        Start symbol: file. *)

  | MenhirState65 : (('s, _menhir_box_file) _menhir_cell1_Files, _menhir_box_file) _menhir_state
    (** State 65.
        Stack shape : Files.
        Start symbol: file. *)

  | MenhirState67 : (('s, _menhir_box_file) _menhir_cell1_Directory, _menhir_box_file) _menhir_state
    (** State 67.
        Stack shape : Directory.
        Start symbol: file. *)

  | MenhirState69 : (('s, _menhir_box_file) _menhir_cell1_Before, _menhir_box_file) _menhir_state
    (** State 69.
        Stack shape : Before.
        Start symbol: file. *)

  | MenhirState71 : (('s, _menhir_box_file) _menhir_cell1_After, _menhir_box_file) _menhir_state
    (** State 71.
        Stack shape : After.
        Start symbol: file. *)

  | MenhirState76 : (('s, _menhir_box_file) _menhir_cell1_mdl_def _menhir_cell0_option_Semi_, _menhir_box_file) _menhir_state
    (** State 76.
        Stack shape : mdl_def option(Semi).
        Start symbol: file. *)

  | MenhirState86 : (('s, _menhir_box_file) _menhir_cell1_def, _menhir_box_file) _menhir_state
    (** State 86.
        Stack shape : def.
        Start symbol: file. *)


and ('s, 'r) _menhir_cell1_atomic_exp = 
  | MenhirCell1_atomic_exp of 's * ('s, 'r) _menhir_state * (Project.exp Project.spanned) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_def = 
  | MenhirCell1_def of 's * ('s, 'r) _menhir_state * (Project.def Project.spanned)

and ('s, 'r) _menhir_cell1_exp = 
  | MenhirCell1_exp of 's * ('s, 'r) _menhir_state * (Project.exp Project.spanned) * Lexing.position

and ('s, 'r) _menhir_cell1_mdl_def = 
  | MenhirCell1_mdl_def of 's * ('s, 'r) _menhir_state * (Project.mdl_def Project.spanned)

and ('s, 'r) _menhir_cell1_op_exp = 
  | MenhirCell1_op_exp of 's * ('s, 'r) _menhir_state * (Project.exp Project.spanned) * Lexing.position

and 's _menhir_cell0_option_Semi_ = 
  | MenhirCell0_option_Semi_ of 's * (unit option)

and ('s, 'r) _menhir_cell1_slash_exp = 
  | MenhirCell1_slash_exp of 's * ('s, 'r) _menhir_state * (Project.exp Project.spanned) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_After = 
  | MenhirCell1_After of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Before = 
  | MenhirCell1_Before of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Comma = 
  | MenhirCell1_Comma of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Directory = 
  | MenhirCell1_Directory of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Files = 
  | MenhirCell1_Files of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Id = 
  | MenhirCell1_Id of 's * ('s, 'r) _menhir_state * (
# 63 "src/lib/project_parser.mly"
       (string)
# 276 "src/lib/project_parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_Id = 
  | MenhirCell0_Id of 's * (
# 63 "src/lib/project_parser.mly"
       (string)
# 283 "src/lib/project_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IdLcurly = 
  | MenhirCell1_IdLcurly of 's * ('s, 'r) _menhir_state * (
# 64 "src/lib/project_parser.mly"
       (string * Lexing.position)
# 290 "src/lib/project_parser.ml"
) * Lexing.position

and ('s, 'r) _menhir_cell1_If = 
  | MenhirCell1_If of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Lcurly = 
  | MenhirCell1_Lcurly of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_Lparen = 
  | MenhirCell1_Lparen of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_Lparen = 
  | MenhirCell0_Lparen of 's * Lexing.position

and ('s, 'r) _menhir_cell1_Lsquare = 
  | MenhirCell1_Lsquare of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Requires = 
  | MenhirCell1_Requires of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Test = 
  | MenhirCell1_Test of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_Variable = 
  | MenhirCell1_Variable of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_file = 
  | MenhirBox_file of (Project.def Project.spanned list) [@@unboxed]

let _menhir_action_01 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 125 "src/lib/project_parser.mly"
    ( span (E_value (bool_value true)) _startpos _endpos )
# 327 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_02 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 127 "src/lib/project_parser.mly"
    ( span (E_value (bool_value false)) _startpos _endpos )
# 337 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_03 =
  fun _endpos_fid_ _startpos_fid_ fid ->
    let _endpos = _endpos_fid_ in
    let _startpos = _startpos_fid_ in
    (
# 129 "src/lib/project_parser.mly"
    ( span (E_file (fst fid, snd fid)) _startpos _endpos )
# 347 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_04 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 131 "src/lib/project_parser.mly"
    ( span (E_id id) _startpos _endpos )
# 357 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_05 =
  fun _endpos__4_ _startpos_id_ args id ->
    let _endpos = _endpos__4_ in
    let _startpos = _startpos_id_ in
    (
# 133 "src/lib/project_parser.mly"
    ( let (x, xs) = args in span (E_app (id, x :: xs)) _startpos _endpos )
# 367 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_06 =
  fun _endpos__3_ _startpos_id_ id ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos_id_ in
    (
# 135 "src/lib/project_parser.mly"
    ( span (E_app (id, [])) _startpos _endpos )
# 377 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_07 =
  fun _endpos_v_ _startpos_v_ v ->
    let _endpos = _endpos_v_ in
    let _startpos = _startpos_v_ in
    (
# 137 "src/lib/project_parser.mly"
    ( span (E_var v) _startpos _endpos )
# 387 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_08 =
  fun _endpos_s_ _startpos_s_ s ->
    let _endpos = _endpos_s_ in
    let _startpos = _startpos_s_ in
    (
# 139 "src/lib/project_parser.mly"
    ( span (E_string s) _startpos _endpos )
# 397 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_09 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 141 "src/lib/project_parser.mly"
    ( span E_parent _startpos _endpos )
# 407 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_10 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos__1_ in
    (
# 143 "src/lib/project_parser.mly"
    ( span (E_list []) _startpos _endpos )
# 417 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_11 =
  fun _endpos__3_ _startpos__1_ es ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 145 "src/lib/project_parser.mly"
    ( span (E_list es) _startpos _endpos )
# 427 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_12 =
  fun e ->
    (
# 147 "src/lib/project_parser.mly"
    ( e )
# 435 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_13 =
  fun _endpos_ids_ _startpos__1_ ids ->
    let _endpos = _endpos_ids_ in
    let _startpos = _startpos__1_ in
    (
# 176 "src/lib/project_parser.mly"
    ( span (Def_test ids) _startpos _endpos )
# 445 "src/lib/project_parser.ml"
     : (Project.def Project.spanned))

let _menhir_action_14 =
  fun _endpos_e_ _endpos_id_ _startpos__1_ _startpos_id_ e id ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 178 "src/lib/project_parser.mly"
    ( span (Def_var (span id _startpos_id_ _endpos_id_, e)) _startpos _endpos )
# 455 "src/lib/project_parser.ml"
     : (Project.def Project.spanned))

let _menhir_action_15 =
  fun _endpos_m_ _startpos_m_ m ->
    let _endpos = _endpos_m_ in
    let _startpos = _startpos_m_ in
    (
# 180 "src/lib/project_parser.mly"
    ( span (Def_module m) _startpos _endpos )
# 465 "src/lib/project_parser.ml"
     : (Project.def Project.spanned))

let _menhir_action_16 =
  fun xs ->
    (
# 151 "src/lib/project_parser.mly"
    ( D_requires xs )
# 473 "src/lib/project_parser.ml"
     : (Project.dependency))

let _menhir_action_17 =
  fun xs ->
    (
# 153 "src/lib/project_parser.mly"
    ( D_after xs )
# 481 "src/lib/project_parser.ml"
     : (Project.dependency))

let _menhir_action_18 =
  fun xs ->
    (
# 155 "src/lib/project_parser.mly"
    ( D_before xs )
# 489 "src/lib/project_parser.ml"
     : (Project.dependency))

let _menhir_action_19 =
  fun e ->
    (
# 75 "src/lib/project_parser.mly"
    ( e )
# 497 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_20 =
  fun _endpos_e_ _startpos__1_ e i t ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 77 "src/lib/project_parser.mly"
    ( span (E_if (i, t, e)) _startpos _endpos )
# 507 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_21 =
  fun es ->
    (
# 93 "src/lib/project_parser.mly"
    ( es )
# 515 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned Project.non_empty))

let _menhir_action_22 =
  fun es ->
    (
# 95 "src/lib/project_parser.mly"
    ( es )
# 523 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned Project.non_empty))

let _menhir_action_23 =
  fun x ->
    (
# 81 "src/lib/project_parser.mly"
    ( [x] )
# 531 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned list))

let _menhir_action_24 =
  fun x xs ->
    (
# 83 "src/lib/project_parser.mly"
    ( x :: xs )
# 539 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned list))

let _menhir_action_25 =
  fun x ->
    (
# 87 "src/lib/project_parser.mly"
    ( (x, []) )
# 547 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned Project.non_empty))

let _menhir_action_26 =
  fun x xs ->
    (
# 89 "src/lib/project_parser.mly"
    ( (x, xs) )
# 555 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned Project.non_empty))

let _menhir_action_27 =
  fun defs ->
    (
# 184 "src/lib/project_parser.mly"
    ( defs )
# 563 "src/lib/project_parser.ml"
     : (Project.def Project.spanned list))

let _menhir_action_28 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 571 "src/lib/project_parser.ml"
     : (Project.def Project.spanned list))

let _menhir_action_29 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 579 "src/lib/project_parser.ml"
     : (Project.def Project.spanned list))

let _menhir_action_30 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 587 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned list))

let _menhir_action_31 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 595 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned list))

let _menhir_action_32 =
  fun _endpos__3_ _startpos_name_ name xs ->
    let defs = 
# 241 "<standard.mly>"
    ( xs )
# 603 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos__3_ in
    let _startpos = _startpos_name_ in
    (
# 169 "src/lib/project_parser.mly"
    ( { name = span (fst name) _startpos_name_ (snd name);
        defs;
        span = (_startpos, _endpos)
      } : mdl )
# 613 "src/lib/project_parser.ml"
     : (Project.mdl))

let _menhir_action_33 =
  fun _endpos_d_ _startpos_d_ d ->
    let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    (
# 159 "src/lib/project_parser.mly"
    ( span (M_dep d) _startpos _endpos )
# 623 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned))

let _menhir_action_34 =
  fun _endpos_m_ _startpos_m_ m ->
    let _endpos = _endpos_m_ in
    let _startpos = _startpos_m_ in
    (
# 161 "src/lib/project_parser.mly"
    ( span (M_module m) _startpos _endpos )
# 633 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned))

let _menhir_action_35 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 163 "src/lib/project_parser.mly"
    ( span (M_directory e) _startpos _endpos )
# 643 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned))

let _menhir_action_36 =
  fun _endpos_es_ _startpos__1_ es ->
    let _endpos = _endpos_es_ in
    let _startpos = _startpos__1_ in
    (
# 165 "src/lib/project_parser.mly"
    ( span (M_files es) _startpos _endpos )
# 653 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned))

let _menhir_action_37 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 661 "src/lib/project_parser.ml"
     : (string list))

let _menhir_action_38 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 669 "src/lib/project_parser.ml"
     : (string list))

let _menhir_action_39 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let o = 
# 99 "src/lib/project_parser.mly"
    ( "==" )
# 677 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 113 "src/lib/project_parser.mly"
    ( span (E_op (lhs, o, rhs)) _startpos _endpos )
# 684 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_40 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let o = 
# 101 "src/lib/project_parser.mly"
    ( "<=" )
# 692 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 113 "src/lib/project_parser.mly"
    ( span (E_op (lhs, o, rhs)) _startpos _endpos )
# 699 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_41 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let o = 
# 103 "src/lib/project_parser.mly"
    ( ">=" )
# 707 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 113 "src/lib/project_parser.mly"
    ( span (E_op (lhs, o, rhs)) _startpos _endpos )
# 714 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_42 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let o = 
# 105 "src/lib/project_parser.mly"
    ( ">" )
# 722 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 113 "src/lib/project_parser.mly"
    ( span (E_op (lhs, o, rhs)) _startpos _endpos )
# 729 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_43 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let o = 
# 107 "src/lib/project_parser.mly"
    ( "<" )
# 737 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 113 "src/lib/project_parser.mly"
    ( span (E_op (lhs, o, rhs)) _startpos _endpos )
# 744 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_44 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let o = 
# 109 "src/lib/project_parser.mly"
    ( "!=" )
# 752 "src/lib/project_parser.ml"
     in
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 113 "src/lib/project_parser.mly"
    ( span (E_op (lhs, o, rhs)) _startpos _endpos )
# 759 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_45 =
  fun e ->
    (
# 115 "src/lib/project_parser.mly"
    ( e )
# 767 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_46 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 775 "src/lib/project_parser.ml"
     : (unit option))

let _menhir_action_47 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 783 "src/lib/project_parser.ml"
     : (unit option))

let _menhir_action_48 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 791 "src/lib/project_parser.ml"
     : (unit option))

let _menhir_action_49 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 799 "src/lib/project_parser.ml"
     : (unit option))

let _menhir_action_50 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 807 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned list))

let _menhir_action_51 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 815 "src/lib/project_parser.ml"
     : (Project.mdl_def Project.spanned list))

let _menhir_action_52 =
  fun _endpos_rhs_ _startpos_lhs_ lhs rhs ->
    let _endpos = _endpos_rhs_ in
    let _startpos = _startpos_lhs_ in
    (
# 119 "src/lib/project_parser.mly"
    ( span (E_op (lhs, "/", rhs)) _startpos _endpos )
# 825 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_action_53 =
  fun e ->
    (
# 121 "src/lib/project_parser.mly"
    ( e )
# 833 "src/lib/project_parser.ml"
     : (Project.exp Project.spanned))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | After ->
        "After"
    | Before ->
        "Before"
    | Comma ->
        "Comma"
    | Directory ->
        "Directory"
    | DotDot ->
        "DotDot"
    | Else ->
        "Else"
    | Eof ->
        "Eof"
    | Eq ->
        "Eq"
    | EqEq ->
        "EqEq"
    | ExclEq ->
        "ExclEq"
    | False ->
        "False"
    | FileId _ ->
        "FileId"
    | Files ->
        "Files"
    | Gt ->
        "Gt"
    | GtEq ->
        "GtEq"
    | Id _ ->
        "Id"
    | IdLcurly _ ->
        "IdLcurly"
    | If ->
        "If"
    | Lcurly ->
        "Lcurly"
    | Lparen ->
        "Lparen"
    | Lsquare ->
        "Lsquare"
    | Lt ->
        "Lt"
    | LtEq ->
        "LtEq"
    | Rcurly ->
        "Rcurly"
    | Requires ->
        "Requires"
    | Rparen ->
        "Rparen"
    | Rsquare ->
        "Rsquare"
    | Semi ->
        "Semi"
    | Slash ->
        "Slash"
    | String _ ->
        "String"
    | Test ->
        "Test"
    | Then ->
        "Then"
    | True ->
        "True"
    | Var _ ->
        "Var"
    | Variable ->
        "Variable"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_83 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let defs = _v in
      let _v = _menhir_action_27 defs in
      MenhirBox_file _v
  
  let rec _menhir_run_87 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_def -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let MenhirCell1_def (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_29 x xs in
      _menhir_goto_list_def_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_def_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState86 ->
          _menhir_run_87 _menhir_stack _v
      | MenhirState00 ->
          _menhir_run_83 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Variable (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Id _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_Id (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Eq ->
              let _menhir_s = MenhirState03 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Var _v ->
                  _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | True ->
                  _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | String _v ->
                  _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | Lsquare ->
                  _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Lparen ->
                  _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | If ->
                  _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Id _v ->
                  _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FileId _v ->
                  _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | False ->
                  _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | DotDot ->
                  _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_07 _endpos_v_ _startpos_v_ v in
      _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_v_ _v _menhir_s _tok
  
  and _menhir_goto_atomic_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Slash ->
          let _menhir_stack = MenhirCell1_atomic_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState21 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | After | Before | Comma | Directory | Else | Eof | EqEq | ExclEq | Files | Gt | GtEq | IdLcurly _ | Lt | LtEq | Rcurly | Requires | Rparen | Rsquare | Semi | Test | Then | Variable ->
          let (_endpos_e_, _startpos_e_, e) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_53 e in
          _menhir_goto_slash_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_01 _endpos__1_ _startpos__1_ in
      _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_s_, _startpos_s_, s) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_08 _endpos_s_ _startpos_s_ s in
      _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos_s_ _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07
      | True ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | String _v ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07
      | Rsquare ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos__1_, _endpos__2_) = (_startpos, _endpos) in
          let _v = _menhir_action_10 _endpos__2_ _startpos__1_ in
          _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | Lsquare ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | Lparen ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | If ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | Id _v ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07
      | FileId _v ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07
      | False ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | DotDot ->
          let _menhir_stack = MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Lparen (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState09 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_If (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState10 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lparen ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v_1 ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState12
          | True ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
          | String _v_2 ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState12
          | Rparen ->
              let _endpos_3 = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_startpos_id_, id, _endpos__3_) = (_startpos, _v, _endpos_3) in
              let _v = _menhir_action_06 _endpos__3_ _startpos_id_ id in
              _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos_id_ _v _menhir_s _tok
          | Lsquare ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
          | Lparen ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
          | If ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
          | Id _v_4 ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState12
          | FileId _v_5 ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState12
          | False ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
          | DotDot ->
              let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_Lparen (_menhir_stack, _startpos_0) in
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
          | _ ->
              _eRR ())
      | After | Before | Comma | Directory | Else | Eof | EqEq | ExclEq | Files | Gt | GtEq | IdLcurly _ | Lt | LtEq | Rcurly | Requires | Rparen | Rsquare | Semi | Slash | Test | Then | Variable ->
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_04 _endpos_id_ _startpos_id_ id in
          _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_fid_, _startpos_fid_, fid) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_03 _endpos_fid_ _startpos_fid_ fid in
      _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_fid_ _startpos_fid_ _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_02 _endpos__1_ _startpos__1_ in
      _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_09 _endpos__1_ _startpos__1_ in
      _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_slash_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState31 ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState29 ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState27 ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState21 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState71 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState69 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState67 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState65 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState59 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState45 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState40 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_32 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_slash_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_slash_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_39 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _v _menhir_s _tok
  
  and _menhir_goto_op_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState45 ->
          _menhir_run_46 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState71 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState69 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState67 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState65 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState59 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState40 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_46 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_If, _menhir_box_file) _menhir_cell1_exp as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_op_exp (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Else ->
          let _menhir_s = MenhirState47 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | If ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_33 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_19 e in
      _menhir_goto_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_goto_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState67 ->
          _menhir_run_68 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState03 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState09 ->
          _menhir_run_49 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState47 ->
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState10 ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState40 ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState37 ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState71 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState69 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState65 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState59 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_68 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Directory -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Directory (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_35 _endpos_e_ _startpos__1_ e in
      _menhir_goto_mdl_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_mdl_def : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Semi ->
          let _menhir_stack = MenhirCell1_mdl_def (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let x = () in
          let _v = _menhir_action_49 x in
          _menhir_goto_option_Semi_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | After | Before | Directory | Files | IdLcurly _ | Requires ->
          let _menhir_stack = MenhirCell1_mdl_def (_menhir_stack, _menhir_s, _v) in
          let _v = _menhir_action_48 () in
          _menhir_goto_option_Semi_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | Rcurly ->
          let x = _v in
          let _v = _menhir_action_50 x in
          _menhir_goto_separated_nonempty_list_option_Semi__mdl_def_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_Semi_ : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_mdl_def -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_option_Semi_ (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | Requires ->
          _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState76
      | IdLcurly _v_0 ->
          _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState76
      | Files ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState76
      | Directory ->
          _menhir_run_67 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState76
      | Before ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState76
      | After ->
          _menhir_run_71 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState76
      | _ ->
          _eRR ()
  
  and _menhir_run_59 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Requires (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState59 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lcurly ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_60 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Lcurly (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState60 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_58 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IdLcurly (_menhir_stack, _menhir_s, _v, _startpos) in
      let _menhir_s = MenhirState58 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Requires ->
          _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IdLcurly _v ->
          _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Files ->
          _menhir_run_65 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Directory ->
          _menhir_run_67 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Before ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | After ->
          _menhir_run_71 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Rcurly ->
          let _v = _menhir_action_30 () in
          _menhir_goto_loption_separated_nonempty_list_option_Semi__mdl_def__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_65 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Files (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState65 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lcurly ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_67 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Directory (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState67 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_69 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Before (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState69 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lcurly ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_71 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_After (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState71 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | True ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | String _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Lsquare ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lparen ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lcurly ->
          _menhir_run_60 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | If ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Id _v ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FileId _v ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | False ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DotDot ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_option_Semi__mdl_def__ : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_IdLcurly -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_IdLcurly (_menhir_stack, _menhir_s, name, _startpos_name_) = _menhir_stack in
      let (xs, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_32 _endpos__3_ _startpos_name_ name xs in
      _menhir_goto_mdl _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos_name_ _v _menhir_s _tok
  
  and _menhir_goto_mdl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState86 ->
          _menhir_run_82 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_82 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState58 ->
          _menhir_run_78 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState76 ->
          _menhir_run_78 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_82 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_m_, _startpos_m_, m) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_15 _endpos_m_ _startpos_m_ m in
      _menhir_goto_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_def : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_def (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Variable ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState86
      | Test ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState86
      | IdLcurly _v_0 ->
          _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState86
      | Eof ->
          let _v_1 = _menhir_action_28 () in
          _menhir_run_87 _menhir_stack _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_54 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_Test (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState54 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Id _v ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_55 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Id _v_0 ->
          let _menhir_stack = MenhirCell1_Id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState55
      | Eof | IdLcurly _ | Test | Variable ->
          let (_endpos_x_, x) = (_endpos, _v) in
          let _v = _menhir_action_37 x in
          _menhir_goto_nonempty_list_Id_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_Id_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState54 ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState55 ->
          _menhir_run_56 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_57 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Test -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Test (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_ids_, ids) = (_endpos, _v) in
      let _v = _menhir_action_13 _endpos_ids_ _startpos__1_ ids in
      _menhir_goto_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_56 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Id -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Id (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_38 x xs in
      _menhir_goto_nonempty_list_Id_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_78 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_m_, _startpos_m_, m) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_34 _endpos_m_ _startpos_m_ m in
      _menhir_goto_mdl_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_separated_nonempty_list_option_Semi__mdl_def_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState76 ->
          _menhir_run_77 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState58 ->
          _menhir_run_73 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_77 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_mdl_def _menhir_cell0_option_Semi_ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell0_option_Semi_ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_mdl_def (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_51 x xs in
      _menhir_goto_separated_nonempty_list_option_Semi__mdl_def_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_73 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_IdLcurly -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_31 x in
      _menhir_goto_loption_separated_nonempty_list_option_Semi__mdl_def__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_53 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Variable _menhir_cell0_Id -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_Id (_menhir_stack, id, _startpos_id_, _endpos_id_) = _menhir_stack in
      let MenhirCell1_Variable (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_14 _endpos_e_ _endpos_id_ _startpos__1_ _startpos_id_ e id in
      _menhir_goto_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_49 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Lparen -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Rparen ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_Lparen (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, e) = (_endpos_0, _v) in
          let _v = _menhir_action_12 e in
          _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_48 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_If, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_cell1_op_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_op_exp (_menhir_stack, _, t, _) = _menhir_stack in
      let MenhirCell1_exp (_menhir_stack, _, i, _) = _menhir_stack in
      let MenhirCell1_If (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_20 _endpos_e_ _startpos__1_ e i t in
      _menhir_goto_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_44 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_If as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Then ->
          let _menhir_s = MenhirState45 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_39 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Comma ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v_1 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState40
          | True ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | String _v_2 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState40
          | Lsquare ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | Lparen ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | If ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | Id _v_3 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState40
          | FileId _v_4 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState40
          | False ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | DotDot ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState39, _endpos_0) in
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState40
          | After | Before | Directory | Files | IdLcurly _ | Rcurly | Requires | Rparen | Rsquare | Semi ->
              let _ =
                let x = () in
                _menhir_action_47 x
              in
              _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_0 _tok
          | _ ->
              _eRR ())
      | After | Before | Directory | Files | IdLcurly _ | Rcurly | Requires | Rparen | Rsquare | Semi ->
          let _ = _menhir_action_46 () in
          _menhir_run_42 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_42 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_exp (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_23 x in
      _menhir_goto_exp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _menhir_s _tok
  
  and _menhir_goto_exp_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState07 ->
          _menhir_run_51 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState40 ->
          _menhir_run_41 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState37 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_51 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Lsquare -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Rsquare ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_Lsquare (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, es) = (_endpos_0, _v) in
          let _v = _menhir_action_11 _endpos__3_ _startpos__1_ es in
          _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_41 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_cell1_Comma -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Comma (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_exp (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_24 x xs in
      _menhir_goto_exp_list _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_38 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_exp, _menhir_box_file) _menhir_cell1_Comma -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Comma (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_exp (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_26 x xs in
      _menhir_goto_exp_non_empty _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_goto_exp_non_empty : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState71 ->
          _menhir_run_63 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState69 ->
          _menhir_run_63 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState65 ->
          _menhir_run_63 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState59 ->
          _menhir_run_63 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState60 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState12 ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_63 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_21 es in
      _menhir_goto_exp_comma_block _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_es_ _v _menhir_s _tok
  
  and _menhir_goto_exp_comma_block : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState71 ->
          _menhir_run_72 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState69 ->
          _menhir_run_70 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState65 ->
          _menhir_run_66 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState59 ->
          _menhir_run_64 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_72 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_After -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_After (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_17 xs in
      _menhir_goto_dependency _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_dependency : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_d_, _startpos_d_, d) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_33 _endpos_d_ _startpos_d_ d in
      _menhir_goto_mdl_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_70 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Before -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Before (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_18 xs in
      _menhir_goto_dependency _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_66 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Files -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Files (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_es_, es) = (_endpos, _v) in
      let _v = _menhir_action_36 _endpos_es_ _startpos__1_ es in
      _menhir_goto_mdl_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_64 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Requires -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_Requires (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_16 xs in
      _menhir_goto_dependency _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_61 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Lcurly -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Rcurly ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_Lcurly (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_endpos__3_, es) = (_endpos_0, _v) in
          let _v = _menhir_action_22 es in
          _menhir_goto_exp_comma_block _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_34 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_Id _menhir_cell0_Lparen -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | Rparen ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_Lparen (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_Id (_menhir_stack, _menhir_s, id, _startpos_id_, _) = _menhir_stack in
          let (_endpos__4_, args) = (_endpos_0, _v) in
          let _v = _menhir_action_05 _endpos__4_ _startpos_id_ args id in
          _menhir_goto_atomic_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | Comma ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v_1 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState37
          | True ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | String _v_2 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState37
          | Lsquare ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | Lparen ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | If ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | Id _v_3 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState37
          | FileId _v_4 ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState37
          | False ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | DotDot ->
              let _menhir_stack = MenhirCell1_Comma (_menhir_stack, MenhirState36, _endpos_0) in
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState37
          | After | Before | Directory | Files | IdLcurly _ | Rcurly | Requires | Rparen | Semi ->
              let _ =
                let x = () in
                _menhir_action_47 x
              in
              _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_0 _tok
          | _ ->
              _eRR ())
      | After | Before | Directory | Files | IdLcurly _ | Rcurly | Requires | Rparen | Semi ->
          let _ = _menhir_action_46 () in
          _menhir_run_43 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_43 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_exp -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_exp (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let _endpos__2_ = _endpos in
      let _v = _menhir_action_25 x in
      _menhir_goto_exp_non_empty _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _v _menhir_s _tok
  
  and _menhir_run_30 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_slash_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_slash_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_44 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _v _menhir_s _tok
  
  and _menhir_run_28 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_slash_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_slash_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_42 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _v _menhir_s _tok
  
  and _menhir_run_26 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_slash_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_slash_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_40 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _v _menhir_s _tok
  
  and _menhir_run_24 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_slash_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_slash_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_43 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _v _menhir_s _tok
  
  and _menhir_run_22 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_atomic_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_atomic_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_52 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_slash_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _startpos_lhs_ _v _menhir_s _tok
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_slash_exp -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_slash_exp (_menhir_stack, _menhir_s, lhs, _startpos_lhs_, _) = _menhir_stack in
      let (_endpos_rhs_, rhs) = (_endpos, _v) in
      let _v = _menhir_action_41 _endpos_rhs_ _startpos_lhs_ lhs rhs in
      _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_rhs_ _v _menhir_s _tok
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LtEq ->
          let _menhir_stack = MenhirCell1_slash_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState18 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | Lt ->
          let _menhir_stack = MenhirCell1_slash_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState23 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | GtEq ->
          let _menhir_stack = MenhirCell1_slash_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState25 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | Gt ->
          let _menhir_stack = MenhirCell1_slash_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState27 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ExclEq ->
          let _menhir_stack = MenhirCell1_slash_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState29 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EqEq ->
          let _menhir_stack = MenhirCell1_slash_exp (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState31 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | True ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | String _v ->
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Lsquare ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lparen ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Id _v ->
              _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FileId _v ->
              _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | False ->
              _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DotDot ->
              _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | After | Before | Comma | Directory | Else | Eof | Files | IdLcurly _ | Rcurly | Requires | Rparen | Rsquare | Semi | Test | Then | Variable ->
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_45 e in
          _menhir_goto_op_exp _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Variable ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | Test ->
          _menhir_run_54 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | IdLcurly _v ->
          _menhir_run_58 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00
      | Eof ->
          let _v = _menhir_action_28 () in
          _menhir_run_83 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_file v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
