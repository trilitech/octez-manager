open Import
module type Intf  =
  sig
    [@@@ocaml.text "{2 Value description}"]
    val value_description :
      loc:Location.t ->
        name:string loc ->
          type_:core_type -> prim:string list -> value_description[@@ocaml.doc
                                                                    " [value_description] constructs an {! Ast.value_description}\n\n{b Example OCaml}\n\n Values of type {!value_description} represents:\n\n    - [val x: T], when {{!value_description.pval_prim} [pval_prim]} is [[]]\n    - [external x: T = \"s1\" ... \"sn\"] when\n      {{!value_description.pval_prim} [pval_prim]} is [[\"s1\";...\"sn\"]]  "]
    [@@@ocaml.text "{2 Value binding}"]
    val value_binding :
      loc:Location.t -> pat:pattern -> expr:expression -> value_binding
    [@@ocaml.doc " [value_binding] constructs an {! Ast.value_binding}\n\n "]
    [@@@ocaml.text "{2 Type extension}"]
    val type_extension :
      loc:Location.t ->
        path:longident loc ->
          params:(core_type * (variance * injectivity)) list ->
            constructors:extension_constructor list ->
              private_:private_flag -> type_extension[@@ocaml.doc
                                                       " [type_extension] constructs an {! Ast.type_extension}\n\n{b Example OCaml}\n\n Definition of new extensions constructors for the extensive sum type [t]\n    ([type t += ...]).  "]
    [@@@ocaml.text "{2 Type exception}"]
    val type_exception :
      loc:Location.t -> extension_constructor -> type_exception[@@ocaml.doc
                                                                 " [type_exception] constructs an {! Ast.type_exception}\n\n{b Example OCaml}\n\n Definition of a new exception ([exception E]).  "]
    [@@@ocaml.text "{2 Type declaration}"]
    val type_declaration :
      loc:Location.t ->
        name:string loc ->
          params:(core_type * (variance * injectivity)) list ->
            cstrs:(core_type * core_type * location) list ->
              kind:type_kind ->
                private_:private_flag ->
                  manifest:core_type option -> type_declaration[@@ocaml.doc
                                                                 " [type_declaration] constructs an {! Ast.type_declaration}\n\n{b Example OCaml}\n\n Here are type declarations and their representation, for various\n    {{!type_declaration.ptype_kind} [ptype_kind]} and\n    {{!type_declaration.ptype_manifest} [ptype_manifest]} values:\n\n    - [type t] when [type_kind] is\n      {{!type_kind.Ptype_abstract} [Ptype_abstract]}, and [manifest] is [None],\n    - [type t = T0] when [type_kind] is\n      {{!type_kind.Ptype_abstract} [Ptype_abstract]}, and [manifest] is\n      [Some T0],\n    - [type t = C of T | ...] when [type_kind] is\n      {{!type_kind.Ptype_variant} [Ptype_variant]}, and [manifest] is [None],\n    - [type t = T0 = C of T | ...] when [type_kind] is\n      {{!type_kind.Ptype_variant} [Ptype_variant]}, and [manifest] is [Some T0],\n    - [type t = {l: T; ...}] when [type_kind] is\n      {{!type_kind.Ptype_record} [Ptype_record]}, and [manifest] is [None],\n    - [type t = T0 = {l : T; ...}] when [type_kind] is\n      {{!type_kind.Ptype_record} [Ptype_record]}, and [manifest] is [Some T0],\n    - [type t = ..] when [type_kind] is {{!type_kind.Ptype_open} [Ptype_open]},\n      and [manifest] is [None].  "]
    [@@@ocaml.text "{2 Toplevel directive}"]
    val toplevel_directive :
      loc:Location.t ->
        name:string loc ->
          arg:directive_argument option -> toplevel_directive[@@ocaml.doc
                                                               " [toplevel_directive] constructs an {! Ast.toplevel_directive}\n\n "]
    [@@@ocaml.text "{2 Structure item}"]
    val pstr_extension :
      loc:Location.t -> extension -> attributes -> structure_item[@@ocaml.doc
                                                                   " [pstr_extension] constructs an {! Ast.Pstr_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val pstr_attribute : loc:Location.t -> attribute -> structure_item
    [@@ocaml.doc
      " [pstr_attribute] constructs an {! Ast.Pstr_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val pstr_include :
      loc:Location.t -> include_declaration -> structure_item[@@ocaml.doc
                                                               " [pstr_include] constructs an {! Ast.Pstr_include}\n\n{b Example OCaml}\n\n [include ME]  "]
    val pstr_class_type :
      loc:Location.t -> class_type_declaration list -> structure_item
    [@@ocaml.doc
      " [pstr_class_type] constructs an {! Ast.Pstr_class_type}\n\n{b Example OCaml}\n\n [class type ct1 = ... and ... and ctn = ...]  "]
    val pstr_class :
      loc:Location.t -> class_declaration list -> structure_item[@@ocaml.doc
                                                                  " [pstr_class] constructs an {! Ast.Pstr_class}\n\n{b Example OCaml}\n\n [class c1 = ... and ... and cn = ...]  "]
    val pstr_open : loc:Location.t -> open_declaration -> structure_item
    [@@ocaml.doc
      " [pstr_open] constructs an {! Ast.Pstr_open}\n\n{b Example OCaml}\n\n [open X]  "]
    val pstr_modtype :
      loc:Location.t -> module_type_declaration -> structure_item[@@ocaml.doc
                                                                   " [pstr_modtype] constructs an {! Ast.Pstr_modtype}\n\n{b Example OCaml}\n\n [module type S = MT]  "]
    val pstr_recmodule :
      loc:Location.t -> module_binding list -> structure_item[@@ocaml.doc
                                                               " [pstr_recmodule] constructs an {! Ast.Pstr_recmodule}\n\n{b Example OCaml}\n\n [module rec X1 = ME1 and ... and Xn = MEn]  "]
    val pstr_module : loc:Location.t -> module_binding -> structure_item
    [@@ocaml.doc
      " [pstr_module] constructs an {! Ast.Pstr_module}\n\n{b Example OCaml}\n\n [module X = ME]  "]
    val pstr_exception : loc:Location.t -> type_exception -> structure_item
    [@@ocaml.doc
      " [pstr_exception] constructs an {! Ast.Pstr_exception}\n\n{b Example OCaml}\n\n - [exception C of T]\n          - [exception C = M.X]  "]
    val pstr_typext : loc:Location.t -> type_extension -> structure_item
    [@@ocaml.doc
      " [pstr_typext] constructs an {! Ast.Pstr_typext}\n\n{b Example OCaml}\n\n [type t1 += ...]  "]
    val pstr_type :
      loc:Location.t -> rec_flag -> type_declaration list -> structure_item
    [@@ocaml.doc
      " [pstr_type] constructs an {! Ast.Pstr_type}\n\n{b Example OCaml}\n\n [type t1 = ... and ... and tn = ...]  "]
    val pstr_primitive :
      loc:Location.t -> value_description -> structure_item[@@ocaml.doc
                                                             " [pstr_primitive] constructs an {! Ast.Pstr_primitive}\n\n{b Example OCaml}\n\n - [val x: T]\n          - [external x: T = \"s1\" ... \"sn\" ] "]
    val pstr_value :
      loc:Location.t -> rec_flag -> value_binding list -> structure_item
    [@@ocaml.doc
      " [pstr_value] constructs an {! Ast.Pstr_value}\n\n{b Example OCaml}\n\n [Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:\n\n          - [let P1 = E1 and ... and Pn = EN] when [rec] is\n            {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},\n          - [let rec P1 = E1 and ... and Pn = EN ] when [rec] is\n            {{!Asttypes.rec_flag.Recursive} [Recursive]}.  "]
    val pstr_eval :
      loc:Location.t -> expression -> attributes -> structure_item[@@ocaml.doc
                                                                    " [pstr_eval] constructs an {! Ast.Pstr_eval}\n\n{b Example OCaml}\n\n [E]  "]
    [@@@ocaml.text "{2 Signature item}"]
    val psig_extension :
      loc:Location.t -> extension -> attributes -> signature_item[@@ocaml.doc
                                                                   " [psig_extension] constructs an {! Ast.Psig_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val psig_attribute : loc:Location.t -> attribute -> signature_item
    [@@ocaml.doc
      " [psig_attribute] constructs an {! Ast.Psig_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val psig_class_type :
      loc:Location.t -> class_type_declaration list -> signature_item
    [@@ocaml.doc
      " [psig_class_type] constructs an {! Ast.Psig_class_type}\n\n{b Example OCaml}\n\n [class type ct1 = ... and ... and ctn = ...]  "]
    val psig_class :
      loc:Location.t -> class_description list -> signature_item[@@ocaml.doc
                                                                  " [psig_class] constructs an {! Ast.Psig_class}\n\n{b Example OCaml}\n\n [class c1 : ... and ... and cn : ...]  "]
    val psig_include :
      loc:Location.t -> include_description -> signature_item[@@ocaml.doc
                                                               " [psig_include] constructs an {! Ast.Psig_include}\n\n{b Example OCaml}\n\n [include MT]  "]
    val psig_open : loc:Location.t -> open_description -> signature_item
    [@@ocaml.doc
      " [psig_open] constructs an {! Ast.Psig_open}\n\n{b Example OCaml}\n\n [open X]  "]
    val psig_modtypesubst :
      loc:Location.t -> module_type_declaration -> signature_item[@@ocaml.doc
                                                                   " [psig_modtypesubst] constructs an {! Ast.Psig_modtypesubst}\n\n{b Example OCaml}\n\n [module type S :=  ...]  "]
    val psig_modtype :
      loc:Location.t -> module_type_declaration -> signature_item[@@ocaml.doc
                                                                   " [psig_modtype] constructs an {! Ast.Psig_modtype}\n\n{b Example OCaml}\n\n [module type S = MT] and [module type S]  "]
    val psig_recmodule :
      loc:Location.t -> module_declaration list -> signature_item[@@ocaml.doc
                                                                   " [psig_recmodule] constructs an {! Ast.Psig_recmodule}\n\n{b Example OCaml}\n\n [module rec X1 : MT1 and ... and Xn : MTn]  "]
    val psig_modsubst :
      loc:Location.t -> module_substitution -> signature_item[@@ocaml.doc
                                                               " [psig_modsubst] constructs an {! Ast.Psig_modsubst}\n\n{b Example OCaml}\n\n [module X := M]  "]
    val psig_module : loc:Location.t -> module_declaration -> signature_item
    [@@ocaml.doc
      " [psig_module] constructs an {! Ast.Psig_module}\n\n{b Example OCaml}\n\n [module X = M] and [module X : MT]  "]
    val psig_exception : loc:Location.t -> type_exception -> signature_item
    [@@ocaml.doc
      " [psig_exception] constructs an {! Ast.Psig_exception}\n\n{b Example OCaml}\n\n [exception C of T]  "]
    val psig_typext : loc:Location.t -> type_extension -> signature_item
    [@@ocaml.doc
      " [psig_typext] constructs an {! Ast.Psig_typext}\n\n{b Example OCaml}\n\n [type t1 += ...]  "]
    val psig_typesubst :
      loc:Location.t -> type_declaration list -> signature_item[@@ocaml.doc
                                                                 " [psig_typesubst] constructs an {! Ast.Psig_typesubst}\n\n{b Example OCaml}\n\n [type t1 := ... and ... and tn := ...]  "]
    val psig_type :
      loc:Location.t -> rec_flag -> type_declaration list -> signature_item
    [@@ocaml.doc
      " [psig_type] constructs an {! Ast.Psig_type}\n\n{b Example OCaml}\n\n [type t1 = ... and ... and tn  = ...]  "]
    val psig_value : loc:Location.t -> value_description -> signature_item
    [@@ocaml.doc
      " [psig_value] constructs an {! Ast.Psig_value}\n\n{b Example OCaml}\n\n - [val x: T]\n          - [external x: T = \"s1\" ... \"sn\"]  "]
    [@@@ocaml.text "{2 Row field}"]
    val rinherit : loc:Location.t -> core_type -> row_field[@@ocaml.doc
                                                             " [rinherit] constructs an {! Ast.Rinherit}\n\n{b Example OCaml}\n\n [[ | t ]]  "]
    val rtag :
      loc:Location.t -> label loc -> bool -> core_type list -> row_field
    [@@ocaml.doc
      " [rtag] constructs an {! Ast.Rtag}\n\n{b Example OCaml}\n\n [Rtag(`A, b, l)] represents:\n\n          - [`A] when [b] is [true] and [l] is [[]],\n          - [`A of T] when [b] is [false] and [l] is [[T]],\n          - [`A of T1 & .. & Tn] when [b] is [false] and [l] is [[T1;...Tn]],\n          - [`A of & T1 & .. & Tn] when [b] is [true] and [l] is [[T1;...Tn]].\n\n          - The [bool] field is true if the tag contains a constant (empty)\n            constructor.\n          - [&] occurs when several types are used for the same constructor (see\n            4.2 in the manual)  "]
    [@@@ocaml.text "{2 Position}"]
    val position :
      fname:string -> lnum:int -> bol:int -> cnum:int -> position[@@ocaml.doc
                                                                   " [position] constructs an {! Ast.position}\n\n "]
    [@@@ocaml.text "{2 Pattern}"]
    val ppat_open : loc:Location.t -> longident loc -> pattern -> pattern
    [@@ocaml.doc
      " [ppat_open] constructs an {! Ast.Ppat_open}\n\n{b Example OCaml}\n\n Pattern [M.(P)]  "]
    val ppat_extension : loc:Location.t -> extension -> pattern[@@ocaml.doc
                                                                 " [ppat_extension] constructs an {! Ast.Ppat_extension}\n\n{b Example OCaml}\n\n Pattern [[%id]]  "]
    val ppat_exception : loc:Location.t -> pattern -> pattern[@@ocaml.doc
                                                               " [ppat_exception] constructs an {! Ast.Ppat_exception}\n\n{b Example OCaml}\n\n Pattern [exception P]  "]
    val ppat_unpack : loc:Location.t -> string option loc -> pattern[@@ocaml.doc
                                                                    " [ppat_unpack] constructs an {! Ast.Ppat_unpack}\n\n{b Example OCaml}\n\n [Ppat_unpack(s)] represents:\n\n          - [(module P)] when [s] is [Some \"P\"]\n          - [(module _)] when [s] is [None]\n\n          Note: [(module P : S)] is represented as\n          [Ppat_constraint(Ppat_unpack(Some \"P\"), Ptyp_package S)]  "]
    val ppat_lazy : loc:Location.t -> pattern -> pattern[@@ocaml.doc
                                                          " [ppat_lazy] constructs an {! Ast.Ppat_lazy}\n\n{b Example OCaml}\n\n Pattern [lazy P]  "]
    val ppat_type : loc:Location.t -> longident loc -> pattern[@@ocaml.doc
                                                                " [ppat_type] constructs an {! Ast.Ppat_type}\n\n{b Example OCaml}\n\n Pattern [#tconst]  "]
    val ppat_constraint : loc:Location.t -> pattern -> core_type -> pattern
    [@@ocaml.doc
      " [ppat_constraint] constructs an {! Ast.Ppat_constraint}\n\n{b Example OCaml}\n\n Pattern [(P : T)]  "]
    val ppat_or : loc:Location.t -> pattern -> pattern -> pattern[@@ocaml.doc
                                                                   " [ppat_or] constructs an {! Ast.Ppat_or}\n\n{b Example OCaml}\n\n Pattern [P1 | P2]  "]
    val ppat_array : loc:Location.t -> pattern list -> pattern[@@ocaml.doc
                                                                " [ppat_array] constructs an {! Ast.Ppat_array}\n\n{b Example OCaml}\n\n Pattern [[| P1; ...; Pn |]]  "]
    val ppat_record :
      loc:Location.t ->
        (longident loc * pattern) list -> closed_flag -> pattern[@@ocaml.doc
                                                                  " [ppat_record] constructs an {! Ast.Ppat_record}\n\n{b Example OCaml}\n\n [Ppat_record([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:\n\n          - [{ l1=P1; ...; ln=Pn }] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}\n          - [{ l1=P1; ...; ln=Pn; _}] when [flag] is\n            {{!Asttypes.closed_flag.Open} [Open]}\n\n          Invariant: [n > 0]  "]
    val ppat_variant : loc:Location.t -> label -> pattern option -> pattern
    [@@ocaml.doc
      " [ppat_variant] constructs an {! Ast.Ppat_variant}\n\n{b Example OCaml}\n\n [Ppat_variant(`A, pat)] represents:\n\n          - [`A] when [pat] is [None],\n          - [`A P] when [pat] is [Some P]  "]
    val ppat_construct :
      loc:Location.t ->
        longident loc -> (string loc list * pattern) option -> pattern
    [@@ocaml.doc
      " [ppat_construct] constructs an {! Ast.Ppat_construct}\n\n{b Example OCaml}\n\n [Ppat_construct(C, args)] represents:\n\n          - [C] when [args] is [None],\n          - [C P] when [args] is [Some ([], P)]\n          - [C (P1, ..., Pn)] when [args] is\n            [Some ([], Ppat_tuple [P1; ...; Pn])]\n          - [C (type a b) P] when [args] is [Some ([a; b], P)]  "]
    val ppat_tuple : loc:Location.t -> pattern list -> pattern[@@ocaml.doc
                                                                " [ppat_tuple] constructs an {! Ast.Ppat_tuple}\n\n{b Example OCaml}\n\n Patterns [(P1, ..., Pn)].\n\n          Invariant: [n >= 2]  "]
    val ppat_interval : loc:Location.t -> constant -> constant -> pattern
    [@@ocaml.doc
      " [ppat_interval] constructs an {! Ast.Ppat_interval}\n\n{b Example OCaml}\n\n Patterns such as ['a'..'z'].\n\n          Other forms of interval are recognized by the parser but rejected by\n          the type-checker.  "]
    val ppat_constant : loc:Location.t -> constant -> pattern[@@ocaml.doc
                                                               " [ppat_constant] constructs an {! Ast.Ppat_constant}\n\n{b Example OCaml}\n\n Patterns such as [1], ['a'], [\"true\"], [1.0], [1l], [1L], [1n]  "]
    val ppat_alias : loc:Location.t -> pattern -> string loc -> pattern
    [@@ocaml.doc
      " [ppat_alias] constructs an {! Ast.Ppat_alias}\n\n{b Example OCaml}\n\n An alias pattern such as [P as 'a]  "]
    val ppat_var : loc:Location.t -> string loc -> pattern[@@ocaml.doc
                                                            " [ppat_var] constructs an {! Ast.Ppat_var}\n\n{b Example OCaml}\n\n A variable pattern such as [x]  "]
    val ppat_any : loc:Location.t -> pattern[@@ocaml.doc
                                              " [ppat_any] constructs an {! Ast.Ppat_any}\n\n{b Example OCaml}\n\n The pattern [_].  "]
    [@@@ocaml.text "{2 Object field}"]
    val oinherit : loc:Location.t -> core_type -> object_field[@@ocaml.doc
                                                                " [oinherit] constructs an {! Ast.Oinherit}\n\n "]
    val otag : loc:Location.t -> label loc -> core_type -> object_field
    [@@ocaml.doc " [otag] constructs an {! Ast.Otag}\n\n "]
    [@@@ocaml.text "{2 Module type declaration}"]
    val module_type_declaration :
      loc:Location.t ->
        name:string loc ->
          type_:module_type option -> module_type_declaration[@@ocaml.doc
                                                               " [module_type_declaration] constructs an {! Ast.module_type_declaration}\n\n{b Example OCaml}\n\n Values of type [module_type_declaration] represents:\n\n    - [S = MT],\n    - [S] for abstract module type declaration, when\n      {{!module_type_declaration.pmtd_type} [pmtd_type]} is [None].  "]
    [@@@ocaml.text "{2 Module type}"]
    val pmty_alias : loc:Location.t -> longident loc -> module_type[@@ocaml.doc
                                                                    " [pmty_alias] constructs an {! Ast.Pmty_alias}\n\n{b Example OCaml}\n\n [(module M)]  "]
    val pmty_extension : loc:Location.t -> extension -> module_type[@@ocaml.doc
                                                                    " [pmty_extension] constructs an {! Ast.Pmty_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pmty_typeof : loc:Location.t -> module_expr -> module_type[@@ocaml.doc
                                                                    " [pmty_typeof] constructs an {! Ast.Pmty_typeof}\n\n{b Example OCaml}\n\n [module type of ME]  "]
    val pmty_with :
      loc:Location.t -> module_type -> with_constraint list -> module_type
    [@@ocaml.doc
      " [pmty_with] constructs an {! Ast.Pmty_with}\n\n{b Example OCaml}\n\n [MT with ...]  "]
    val pmty_functor :
      loc:Location.t -> functor_parameter -> module_type -> module_type
    [@@ocaml.doc
      " [pmty_functor] constructs an {! Ast.Pmty_functor}\n\n{b Example OCaml}\n\n [functor(X : MT1) -> MT2]  "]
    val pmty_signature : loc:Location.t -> signature -> module_type[@@ocaml.doc
                                                                    " [pmty_signature] constructs an {! Ast.Pmty_signature}\n\n{b Example OCaml}\n\n [sig ... end]  "]
    val pmty_ident : loc:Location.t -> longident loc -> module_type[@@ocaml.doc
                                                                    " [pmty_ident] constructs an {! Ast.Pmty_ident}\n\n{b Example OCaml}\n\n [Pmty_ident(S)] represents [S]  "]
    [@@@ocaml.text "{2 Module substitution}"]
    val module_substitution :
      loc:Location.t ->
        name:string loc -> manifest:longident loc -> module_substitution
    [@@ocaml.doc
      " [module_substitution] constructs an {! Ast.module_substitution}\n\n{b Example OCaml}\n\n Values of type [module_substitution] represents [S := M]  "]
    [@@@ocaml.text "{2 Module expr}"]
    val pmod_extension : loc:Location.t -> extension -> module_expr[@@ocaml.doc
                                                                    " [pmod_extension] constructs an {! Ast.Pmod_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pmod_unpack : loc:Location.t -> expression -> module_expr[@@ocaml.doc
                                                                   " [pmod_unpack] constructs an {! Ast.Pmod_unpack}\n\n{b Example OCaml}\n\n [(val E)]  "]
    val pmod_constraint :
      loc:Location.t -> module_expr -> module_type -> module_expr[@@ocaml.doc
                                                                   " [pmod_constraint] constructs an {! Ast.Pmod_constraint}\n\n{b Example OCaml}\n\n [(ME : MT)]  "]
    val pmod_apply :
      loc:Location.t -> module_expr -> module_expr -> module_expr[@@ocaml.doc
                                                                   " [pmod_apply] constructs an {! Ast.Pmod_apply}\n\n{b Example OCaml}\n\n [ME1(ME2)]  "]
    val pmod_functor :
      loc:Location.t -> functor_parameter -> module_expr -> module_expr
    [@@ocaml.doc
      " [pmod_functor] constructs an {! Ast.Pmod_functor}\n\n{b Example OCaml}\n\n [functor(X : MT1) -> ME]  "]
    val pmod_structure : loc:Location.t -> structure -> module_expr[@@ocaml.doc
                                                                    " [pmod_structure] constructs an {! Ast.Pmod_structure}\n\n{b Example OCaml}\n\n [struct ... end]  "]
    val pmod_ident : loc:Location.t -> longident loc -> module_expr[@@ocaml.doc
                                                                    " [pmod_ident] constructs an {! Ast.Pmod_ident}\n\n{b Example OCaml}\n\n [X]  "]
    [@@@ocaml.text "{2 Module declaration}"]
    val module_declaration :
      loc:Location.t ->
        name:string option loc -> type_:module_type -> module_declaration
    [@@ocaml.doc
      " [module_declaration] constructs an {! Ast.module_declaration}\n\n{b Example OCaml}\n\n Values of type [module_declaration] represents [S : MT]  "]
    [@@@ocaml.text "{2 Module binding}"]
    val module_binding :
      loc:Location.t ->
        name:string option loc -> expr:module_expr -> module_binding[@@ocaml.doc
                                                                    " [module_binding] constructs an {! Ast.module_binding}\n\n{b Example OCaml}\n\n Values of type [module_binding] represents [module X = ME]  "]
    [@@@ocaml.text "{2 Location}"]
    val location : start:position -> end_:position -> ghost:bool -> location
    [@@ocaml.doc " [location] constructs an {! Ast.location}\n\n "]
    [@@@ocaml.text "{2 Letop}"]
    val letop :
      let_:binding_op -> ands:binding_op list -> body:expression -> letop
    [@@ocaml.doc " [letop] constructs an {! Ast.letop}\n\n "]
    [@@@ocaml.text "{2 Label declaration}"]
    val label_declaration :
      loc:Location.t ->
        name:string loc ->
          mutable_:mutable_flag -> type_:core_type -> label_declaration
    [@@ocaml.doc
      " [label_declaration] constructs an {! Ast.label_declaration}\n\n{b Example OCaml}\n\n - [{ ...; l: T; ... }] when {{!label_declaration.pld_mutable} [pld_mutable]}\n      is {{!Asttypes.mutable_flag.Immutable} [Immutable]},\n    - [{ ...; mutable l: T; ... }] when\n      {{!label_declaration.pld_mutable} [pld_mutable]} is\n      {{!Asttypes.mutable_flag.Mutable} [Mutable]}.\n\n    Note: [T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}.  "]
    [@@@ocaml.text "{2 Extension constructor}"]
    val extension_constructor :
      loc:Location.t ->
        name:string loc ->
          kind:extension_constructor_kind -> extension_constructor[@@ocaml.doc
                                                                    " [extension_constructor] constructs an {! Ast.extension_constructor}\n\n "]
    [@@@ocaml.text "{2 Expression}"]
    val pexp_unreachable : loc:Location.t -> expression[@@ocaml.doc
                                                         " [pexp_unreachable] constructs an {! Ast.Pexp_unreachable}\n\n{b Example OCaml}\n\n [.]  "]
    val pexp_extension : loc:Location.t -> extension -> expression[@@ocaml.doc
                                                                    " [pexp_extension] constructs an {! Ast.Pexp_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pexp_letop : loc:Location.t -> letop -> expression[@@ocaml.doc
                                                            " [pexp_letop] constructs an {! Ast.Pexp_letop}\n\n{b Example OCaml}\n\n - [let* P = E0 in E1]\n          - [let* P0 = E00 and* P1 = E01 in E1]  "]
    val pexp_open :
      loc:Location.t -> open_declaration -> expression -> expression[@@ocaml.doc
                                                                    " [pexp_open] constructs an {! Ast.Pexp_open}\n\n{b Example OCaml}\n\n - [M.(E)]\n          - [let open M in E]\n          - [let open! M in E]  "]
    val pexp_pack : loc:Location.t -> module_expr -> expression[@@ocaml.doc
                                                                 " [pexp_pack] constructs an {! Ast.Pexp_pack}\n\n{b Example OCaml}\n\n [(module ME)].\n\n          [(module ME : S)] is represented as\n          [Pexp_constraint(Pexp_pack ME, Ptyp_package S)]  "]
    val pexp_newtype :
      loc:Location.t -> string loc -> expression -> expression[@@ocaml.doc
                                                                " [pexp_newtype] constructs an {! Ast.Pexp_newtype}\n\n{b Example OCaml}\n\n [fun (type t) -> E]  "]
    val pexp_object : loc:Location.t -> class_structure -> expression
    [@@ocaml.doc
      " [pexp_object] constructs an {! Ast.Pexp_object}\n\n{b Example OCaml}\n\n [object ... end]  "]
    val pexp_poly :
      loc:Location.t -> expression -> core_type option -> expression[@@ocaml.doc
                                                                    " [pexp_poly] constructs an {! Ast.Pexp_poly}\n\n{b Example OCaml}\n\n Used for method bodies.\n\n          Can only be used as the expression under\n          {{!class_field_kind.Cfk_concrete} [Cfk_concrete]} for methods (not\n          values).  "]
    val pexp_lazy : loc:Location.t -> expression -> expression[@@ocaml.doc
                                                                " [pexp_lazy] constructs an {! Ast.Pexp_lazy}\n\n{b Example OCaml}\n\n [lazy E]  "]
    val pexp_assert : loc:Location.t -> expression -> expression[@@ocaml.doc
                                                                  " [pexp_assert] constructs an {! Ast.Pexp_assert}\n\n{b Example OCaml}\n\n [assert E].\n\n          Note: [assert false] is treated in a special way by the type-checker.\n       "]
    val pexp_letexception :
      loc:Location.t -> extension_constructor -> expression -> expression
    [@@ocaml.doc
      " [pexp_letexception] constructs an {! Ast.Pexp_letexception}\n\n{b Example OCaml}\n\n [let exception C in E]  "]
    val pexp_letmodule :
      loc:Location.t ->
        string option loc -> module_expr -> expression -> expression[@@ocaml.doc
                                                                    " [pexp_letmodule] constructs an {! Ast.Pexp_letmodule}\n\n{b Example OCaml}\n\n [let module M = ME in E]  "]
    val pexp_override :
      loc:Location.t -> (label loc * expression) list -> expression[@@ocaml.doc
                                                                    " [pexp_override] constructs an {! Ast.Pexp_override}\n\n{b Example OCaml}\n\n [{< x1 = E1; ...; xn = En >}]  "]
    val pexp_setinstvar :
      loc:Location.t -> label loc -> expression -> expression[@@ocaml.doc
                                                               " [pexp_setinstvar] constructs an {! Ast.Pexp_setinstvar}\n\n{b Example OCaml}\n\n [x <- 2]  "]
    val pexp_new : loc:Location.t -> longident loc -> expression[@@ocaml.doc
                                                                  " [pexp_new] constructs an {! Ast.Pexp_new}\n\n{b Example OCaml}\n\n [new M.c]  "]
    val pexp_send : loc:Location.t -> expression -> label loc -> expression
    [@@ocaml.doc
      " [pexp_send] constructs an {! Ast.Pexp_send}\n\n{b Example OCaml}\n\n [E # m]  "]
    val pexp_coerce :
      loc:Location.t ->
        expression -> core_type option -> core_type -> expression[@@ocaml.doc
                                                                   " [pexp_coerce] constructs an {! Ast.Pexp_coerce}\n\n{b Example OCaml}\n\n [Pexp_coerce(E, from, T)] represents\n\n          - [(E :> T)] when [from] is [None],\n          - [(E : T0 :> T)] when [from] is [Some T0].  "]
    val pexp_constraint :
      loc:Location.t -> expression -> core_type -> expression[@@ocaml.doc
                                                               " [pexp_constraint] constructs an {! Ast.Pexp_constraint}\n\n{b Example OCaml}\n\n [(E : T)]  "]
    val pexp_for :
      loc:Location.t ->
        pattern ->
          expression ->
            expression -> direction_flag -> expression -> expression[@@ocaml.doc
                                                                    " [pexp_for] constructs an {! Ast.Pexp_for}\n\n{b Example OCaml}\n\n [Pexp_for(i, E1, E2, direction, E3)] represents:\n\n          - [for i = E1 to E2 do E3 done] when [direction] is\n            {{!Asttypes.direction_flag.Upto} [Upto]}\n          - [for i = E1 downto E2 do E3 done] when [direction] is\n            {{!Asttypes.direction_flag.Downto} [Downto]}  "]
    val pexp_while : loc:Location.t -> expression -> expression -> expression
    [@@ocaml.doc
      " [pexp_while] constructs an {! Ast.Pexp_while}\n\n{b Example OCaml}\n\n [while E1 do E2 done]  "]
    val pexp_sequence :
      loc:Location.t -> expression -> expression -> expression[@@ocaml.doc
                                                                " [pexp_sequence] constructs an {! Ast.Pexp_sequence}\n\n{b Example OCaml}\n\n [E1; E2]  "]
    val pexp_ifthenelse :
      loc:Location.t ->
        expression -> expression -> expression option -> expression[@@ocaml.doc
                                                                    " [pexp_ifthenelse] constructs an {! Ast.Pexp_ifthenelse}\n\n{b Example OCaml}\n\n [if E1 then E2 else E3]  "]
    val pexp_array : loc:Location.t -> expression list -> expression[@@ocaml.doc
                                                                    " [pexp_array] constructs an {! Ast.Pexp_array}\n\n{b Example OCaml}\n\n [[| E1; ...; En |]]  "]
    val pexp_setfield :
      loc:Location.t ->
        expression -> longident loc -> expression -> expression[@@ocaml.doc
                                                                 " [pexp_setfield] constructs an {! Ast.Pexp_setfield}\n\n{b Example OCaml}\n\n [E1.l <- E2]  "]
    val pexp_field :
      loc:Location.t -> expression -> longident loc -> expression[@@ocaml.doc
                                                                   " [pexp_field] constructs an {! Ast.Pexp_field}\n\n{b Example OCaml}\n\n [E.l]  "]
    val pexp_record :
      loc:Location.t ->
        (longident loc * expression) list -> expression option -> expression
    [@@ocaml.doc
      " [pexp_record] constructs an {! Ast.Pexp_record}\n\n{b Example OCaml}\n\n [Pexp_record([(l1,P1) ; ... ; (ln,Pn)], exp0)] represents\n\n          - [{ l1=P1; ...; ln=Pn }] when [exp0] is [None]\n          - [{ E0 with l1=P1; ...; ln=Pn }] when [exp0] is [Some E0]\n\n          Invariant: [n > 0]  "]
    val pexp_variant :
      loc:Location.t -> label -> expression option -> expression[@@ocaml.doc
                                                                  " [pexp_variant] constructs an {! Ast.Pexp_variant}\n\n{b Example OCaml}\n\n [Pexp_variant(`A, exp)] represents\n\n          - [`A] when [exp] is [None]\n          - [`A E] when [exp] is [Some E]  "]
    val pexp_construct :
      loc:Location.t -> longident loc -> expression option -> expression
    [@@ocaml.doc
      " [pexp_construct] constructs an {! Ast.Pexp_construct}\n\n{b Example OCaml}\n\n [Pexp_construct(C, exp)] represents:\n\n          - [C] when [exp] is [None],\n          - [C E] when [exp] is [Some E],\n          - [C (E1, ..., En)] when [exp] is [Some (Pexp_tuple[E1;...;En])]  "]
    val pexp_tuple : loc:Location.t -> expression list -> expression[@@ocaml.doc
                                                                    " [pexp_tuple] constructs an {! Ast.Pexp_tuple}\n\n{b Example OCaml}\n\n Expressions [(E1, ..., En)]\n\n          Invariant: [n >= 2]  "]
    val pexp_try : loc:Location.t -> expression -> cases -> expression
    [@@ocaml.doc
      " [pexp_try] constructs an {! Ast.Pexp_try}\n\n{b Example OCaml}\n\n [try E0 with P1 -> E1 | ... | Pn -> En]  "]
    val pexp_match : loc:Location.t -> expression -> cases -> expression
    [@@ocaml.doc
      " [pexp_match] constructs an {! Ast.Pexp_match}\n\n{b Example OCaml}\n\n [match E0 with P1 -> E1 | ... | Pn -> En]  "]
    val pexp_apply :
      loc:Location.t ->
        expression -> (arg_label * expression) list -> expression[@@ocaml.doc
                                                                   " [pexp_apply] constructs an {! Ast.Pexp_apply}\n\n{b Example OCaml}\n\n [Pexp_apply(E0, [(l1, E1) ; ... ; (ln, En)])] represents\n          [E0 ~l1:E1 ... ~ln:En]\n\n          [li] can be {{!Asttypes.arg_label.Nolabel} [Nolabel]} (non labeled\n          argument), {{!Asttypes.arg_label.Labelled} [Labelled]} (labelled\n          arguments) or {{!Asttypes.arg_label.Optional} [Optional]} (optional\n          argument).\n\n          Invariant: [n > 0]  "]
    val pexp_fun :
      loc:Location.t ->
        arg_label -> expression option -> pattern -> expression -> expression
    [@@ocaml.doc
      " [pexp_fun] constructs an {! Ast.Pexp_fun}\n\n{b Example OCaml}\n\n [Pexp_fun(lbl, exp0, P, E1)] represents:\n\n          - [fun P -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Nolabel} [Nolabel]} and [exp0] is [None]\n          - [fun ~l:P -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled l]} and [exp0] is [None]\n          - [fun ?l:P -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is [None]\n          - [fun ?l:(P = E0) -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is\n            [Some E0]\n\n          Notes:\n\n          - If [E0] is provided, only\n            {{!Asttypes.arg_label.Optional} [Optional]} is allowed.\n          - [fun P1 P2 .. Pn -> E1] is represented as nested\n            {{!expression_desc.Pexp_fun} [Pexp_fun]}.\n          - [let f P = E] is represented using\n            {{!expression_desc.Pexp_fun} [Pexp_fun]}.  "]
    val pexp_function : loc:Location.t -> cases -> expression[@@ocaml.doc
                                                               " [pexp_function] constructs an {! Ast.Pexp_function}\n\n{b Example OCaml}\n\n [function P1 -> E1 | ... | Pn -> En]  "]
    val pexp_let :
      loc:Location.t ->
        rec_flag -> value_binding list -> expression -> expression[@@ocaml.doc
                                                                    " [pexp_let] constructs an {! Ast.Pexp_let}\n\n{b Example OCaml}\n\n [Pexp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:\n\n          - [let P1 = E1 and ... and Pn = EN in E] when [flag] is\n            {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},\n          - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is\n            {{!Asttypes.rec_flag.Recursive} [Recursive]}.  "]
    val pexp_constant : loc:Location.t -> constant -> expression[@@ocaml.doc
                                                                  " [pexp_constant] constructs an {! Ast.Pexp_constant}\n\n{b Example OCaml}\n\n Expressions constant such as [1], ['a'], [\"true\"], [1.0], [1l], [1L],\n          [1n]  "]
    val pexp_ident : loc:Location.t -> longident loc -> expression[@@ocaml.doc
                                                                    " [pexp_ident] constructs an {! Ast.Pexp_ident}\n\n{b Example OCaml}\n\n Identifiers such as [x] and [M.x]  "]
    [@@@ocaml.text "{2 Directive argument}"]
    val pdir_bool : loc:Location.t -> bool -> directive_argument[@@ocaml.doc
                                                                  " [pdir_bool] constructs an {! Ast.Pdir_bool}\n\n "]
    val pdir_ident : loc:Location.t -> longident -> directive_argument
    [@@ocaml.doc " [pdir_ident] constructs an {! Ast.Pdir_ident}\n\n "]
    val pdir_int :
      loc:Location.t -> string -> char option -> directive_argument[@@ocaml.doc
                                                                    " [pdir_int] constructs an {! Ast.Pdir_int}\n\n "]
    val pdir_string : loc:Location.t -> string -> directive_argument[@@ocaml.doc
                                                                    " [pdir_string] constructs an {! Ast.Pdir_string}\n\n "]
    [@@@ocaml.text "{2 Core type}"]
    val ptyp_extension : loc:Location.t -> extension -> core_type[@@ocaml.doc
                                                                   " [ptyp_extension] constructs an {! Ast.Ptyp_extension}\n\n{b Example OCaml}\n\n [[%id]].  "]
    val ptyp_package : loc:Location.t -> package_type -> core_type[@@ocaml.doc
                                                                    " [ptyp_package] constructs an {! Ast.Ptyp_package}\n\n{b Example OCaml}\n\n [(module S)].  "]
    val ptyp_poly :
      loc:Location.t -> string loc list -> core_type -> core_type[@@ocaml.doc
                                                                   " [ptyp_poly] constructs an {! Ast.Ptyp_poly}\n\n{b Example OCaml}\n\n ['a1 ... 'an. T]\n\n          Can only appear in the following context:\n\n          - As the {!core_type} of a\n            {{!pattern_desc.Ppat_constraint} [Ppat_constraint]} node\n            corresponding to a constraint on a let-binding:\n\n          {[\n            let x : 'a1 ... 'an. T = e ...\n          ]}\n          - Under {{!class_field_kind.Cfk_virtual} [Cfk_virtual]} for methods\n            (not values).\n\n          - As the {!core_type} of a\n            {{!class_type_field_desc.Pctf_method} [Pctf_method]} node.\n\n          - As the {!core_type} of a {{!expression_desc.Pexp_poly} [Pexp_poly]}\n            node.\n\n          - As the {{!label_declaration.pld_type} [pld_type]} field of a\n            {!label_declaration}.\n\n          - As a {!core_type} of a {{!core_type_desc.Ptyp_object} [Ptyp_object]}\n            node.\n\n          - As the {{!value_description.pval_type} [pval_type]} field of a\n            {!value_description}.  "]
    val ptyp_variant :
      loc:Location.t ->
        row_field list -> closed_flag -> label list option -> core_type
    [@@ocaml.doc
      " [ptyp_variant] constructs an {! Ast.Ptyp_variant}\n\n{b Example OCaml}\n\n [Ptyp_variant([`A;`B], flag, labels)] represents:\n\n          - [[ `A|`B ]] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels] is [None],\n          - [[> `A|`B ]] when [flag] is {{!Asttypes.closed_flag.Open} [Open]},\n            and [labels] is [None],\n          - [[< `A|`B ]] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels] is\n            [Some []],\n          - [[< `A|`B > `X `Y ]] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels] is\n            [Some [\"X\";\"Y\"]].  "]
    val ptyp_alias : loc:Location.t -> core_type -> string -> core_type
    [@@ocaml.doc
      " [ptyp_alias] constructs an {! Ast.Ptyp_alias}\n\n{b Example OCaml}\n\n [T as 'a].  "]
    val ptyp_class :
      loc:Location.t -> longident loc -> core_type list -> core_type[@@ocaml.doc
                                                                    " [ptyp_class] constructs an {! Ast.Ptyp_class}\n\n{b Example OCaml}\n\n [Ptyp_class(tconstr, l)] represents:\n\n          - [#tconstr] when [l=[]],\n          - [T #tconstr] when [l=[T]],\n          - [(T1, ..., Tn) #tconstr] when [l=[T1 ; ... ; Tn]].  "]
    val ptyp_object :
      loc:Location.t -> object_field list -> closed_flag -> core_type
    [@@ocaml.doc
      " [ptyp_object] constructs an {! Ast.Ptyp_object}\n\n{b Example OCaml}\n\n [Ptyp_object([ l1:T1; ...; ln:Tn ], flag)] represents:\n\n          - [< l1:T1; ...; ln:Tn >] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]},\n          - [< l1:T1; ...; ln:Tn; .. >] when [flag] is\n            {{!Asttypes.closed_flag.Open} [Open]}.  "]
    val ptyp_constr :
      loc:Location.t -> longident loc -> core_type list -> core_type[@@ocaml.doc
                                                                    " [ptyp_constr] constructs an {! Ast.Ptyp_constr}\n\n{b Example OCaml}\n\n [Ptyp_constr(lident, l)] represents:\n\n          - [tconstr] when [l=[]],\n          - [T tconstr] when [l=[T]],\n          - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].  "]
    val ptyp_tuple : loc:Location.t -> core_type list -> core_type[@@ocaml.doc
                                                                    " [ptyp_tuple] constructs an {! Ast.Ptyp_tuple}\n\n{b Example OCaml}\n\n [Ptyp_tuple([T1 ; ... ; Tn])] represents a product type\n          [T1 * ... * Tn].\n\n          Invariant: [n >= 2].  "]
    val ptyp_arrow :
      loc:Location.t -> arg_label -> core_type -> core_type -> core_type
    [@@ocaml.doc
      " [ptyp_arrow] constructs an {! Ast.Ptyp_arrow}\n\n{b Example OCaml}\n\n [Ptyp_arrow(lbl, T1, T2)] represents:\n\n          - [T1 -> T2] when [lbl] is {{!Asttypes.arg_label.Nolabel} [Nolabel]},\n          - [~l:T1 -> T2] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled]},\n          - [?l:T1 -> T2] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional]}.  "]
    val ptyp_var : loc:Location.t -> string -> core_type[@@ocaml.doc
                                                          " [ptyp_var] constructs an {! Ast.Ptyp_var}\n\n{b Example OCaml}\n\n A type variable such as ['a]  "]
    val ptyp_any : loc:Location.t -> core_type[@@ocaml.doc
                                                " [ptyp_any] constructs an {! Ast.Ptyp_any}\n\n{b Example OCaml}\n\n [_]  "]
    [@@@ocaml.text "{2 Constructor declaration}"]
    val constructor_declaration :
      loc:Location.t ->
        name:string loc ->
          vars:string loc list ->
            args:constructor_arguments ->
              res:core_type option -> constructor_declaration[@@ocaml.doc
                                                               " [constructor_declaration] constructs an {! Ast.constructor_declaration}\n\n "]
    [@@@ocaml.text "{2 Class type field}"]
    val pctf_extension : loc:Location.t -> extension -> class_type_field
    [@@ocaml.doc
      " [pctf_extension] constructs an {! Ast.Pctf_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val pctf_attribute : loc:Location.t -> attribute -> class_type_field
    [@@ocaml.doc
      " [pctf_attribute] constructs an {! Ast.Pctf_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val pctf_constraint :
      loc:Location.t -> (core_type * core_type) -> class_type_field[@@ocaml.doc
                                                                    " [pctf_constraint] constructs an {! Ast.Pctf_constraint}\n\n{b Example OCaml}\n\n [constraint T1 = T2]  "]
    val pctf_method :
      loc:Location.t ->
        (label loc * private_flag * virtual_flag * core_type) ->
          class_type_field[@@ocaml.doc
                            " [pctf_method] constructs an {! Ast.Pctf_method}\n\n{b Example OCaml}\n\n [method x: T]\n\n          Note: [T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}.  "]
    val pctf_val :
      loc:Location.t ->
        (label loc * mutable_flag * virtual_flag * core_type) ->
          class_type_field[@@ocaml.doc
                            " [pctf_val] constructs an {! Ast.Pctf_val}\n\n{b Example OCaml}\n\n [val x: T]  "]
    val pctf_inherit : loc:Location.t -> class_type -> class_type_field
    [@@ocaml.doc
      " [pctf_inherit] constructs an {! Ast.Pctf_inherit}\n\n{b Example OCaml}\n\n [inherit CT]  "]
    [@@@ocaml.text "{2 Class type}"]
    val pcty_open :
      loc:Location.t -> open_description -> class_type -> class_type[@@ocaml.doc
                                                                    " [pcty_open] constructs an {! Ast.Pcty_open}\n\n{b Example OCaml}\n\n [let open M in CT]  "]
    val pcty_extension : loc:Location.t -> extension -> class_type[@@ocaml.doc
                                                                    " [pcty_extension] constructs an {! Ast.Pcty_extension}\n\n{b Example OCaml}\n\n [%id]  "]
    val pcty_arrow :
      loc:Location.t -> arg_label -> core_type -> class_type -> class_type
    [@@ocaml.doc
      " [pcty_arrow] constructs an {! Ast.Pcty_arrow}\n\n{b Example OCaml}\n\n [Pcty_arrow(lbl, T, CT)] represents:\n\n          - [T -> CT] when [lbl] is {{!Asttypes.arg_label.Nolabel} [Nolabel]},\n          - [~l:T -> CT] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled l]},\n          - [?l:T -> CT] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]}.  "]
    val pcty_signature : loc:Location.t -> class_signature -> class_type
    [@@ocaml.doc
      " [pcty_signature] constructs an {! Ast.Pcty_signature}\n\n{b Example OCaml}\n\n [object ... end]  "]
    val pcty_constr :
      loc:Location.t -> longident loc -> core_type list -> class_type
    [@@ocaml.doc
      " [pcty_constr] constructs an {! Ast.Pcty_constr}\n\n{b Example OCaml}\n\n - [c]\n          - [['a1, ..., 'an] c]  "]
    [@@@ocaml.text "{2 Class structure}"]
    val class_structure :
      self:pattern -> fields:class_field list -> class_structure[@@ocaml.doc
                                                                  " [class_structure] constructs an {! Ast.class_structure}\n\n{b Example OCaml}\n\n Values of type {!class_structure} represents:\n\n    - [object(selfpat) ... end]\n    - [object ... end] when {{!class_structure.pcstr_self} [pcstr_self]} is\n      {{!pattern_desc.Ppat_any} [Ppat_any]}  "]
    [@@@ocaml.text "{2 Class signature}"]
    val class_signature :
      self:core_type -> fields:class_type_field list -> class_signature
    [@@ocaml.doc
      " [class_signature] constructs an {! Ast.class_signature}\n\n{b Example OCaml}\n\n Values of type [class_signature] represents:\n\n    - [object('selfpat) ... end]\n    - [object ... end] when {{!class_signature.pcsig_self} [pcsig_self]} is\n      {{!core_type_desc.Ptyp_any} [Ptyp_any]}  "]
    [@@@ocaml.text "{2 Class field}"]
    val pcf_extension : loc:Location.t -> extension -> class_field[@@ocaml.doc
                                                                    " [pcf_extension] constructs an {! Ast.Pcf_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val pcf_attribute : loc:Location.t -> attribute -> class_field[@@ocaml.doc
                                                                    " [pcf_attribute] constructs an {! Ast.Pcf_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val pcf_initializer : loc:Location.t -> expression -> class_field
    [@@ocaml.doc
      " [pcf_initializer] constructs an {! Ast.Pcf_initializer}\n\n{b Example OCaml}\n\n [initializer E]  "]
    val pcf_constraint :
      loc:Location.t -> (core_type * core_type) -> class_field[@@ocaml.doc
                                                                " [pcf_constraint] constructs an {! Ast.Pcf_constraint}\n\n{b Example OCaml}\n\n [constraint T1 = T2]  "]
    val pcf_method :
      loc:Location.t ->
        (label loc * private_flag * class_field_kind) -> class_field[@@ocaml.doc
                                                                    " [pcf_method] constructs an {! Ast.Pcf_method}\n\n{b Example OCaml}\n\n - [method x = E] ([E] can be a\n            {{!expression_desc.Pexp_poly} [Pexp_poly]})\n          - [method virtual x: T] ([T] can be a\n            {{!core_type_desc.Ptyp_poly} [Ptyp_poly]})  "]
    val pcf_val :
      loc:Location.t ->
        (label loc * mutable_flag * class_field_kind) -> class_field[@@ocaml.doc
                                                                    " [pcf_val] constructs an {! Ast.Pcf_val}\n\n{b Example OCaml}\n\n [Pcf_val(x,flag, kind)] represents:\n\n          - [val x = E] when [flag] is\n            {{!Asttypes.mutable_flag.Immutable} [Immutable]} and [kind] is\n            {{!class_field_kind.Cfk_concrete} [Cfk_concrete(Fresh, E)]}\n          - [val virtual x: T] when [flag] is\n            {{!Asttypes.mutable_flag.Immutable} [Immutable]} and [kind] is\n            {{!class_field_kind.Cfk_virtual} [Cfk_virtual(T)]}\n          - [val mutable x = E] when [flag] is\n            {{!Asttypes.mutable_flag.Mutable} [Mutable]} and [kind] is\n            {{!class_field_kind.Cfk_concrete} [Cfk_concrete(Fresh, E)]}\n          - [val mutable virtual x: T] when [flag] is\n            {{!Asttypes.mutable_flag.Mutable} [Mutable]} and [kind] is\n            {{!class_field_kind.Cfk_virtual} [Cfk_virtual(T)]}  "]
    val pcf_inherit :
      loc:Location.t ->
        override_flag -> class_expr -> string loc option -> class_field
    [@@ocaml.doc
      " [pcf_inherit] constructs an {! Ast.Pcf_inherit}\n\n{b Example OCaml}\n\n [Pcf_inherit(flag, CE, s)] represents:\n\n          - [inherit CE] when [flag] is\n            {{!Asttypes.override_flag.Fresh} [Fresh]} and [s] is [None],\n          - [inherit CE as x] when [flag] is\n            {{!Asttypes.override_flag.Fresh} [Fresh]} and [s] is [Some x],\n          - [inherit! CE] when [flag] is\n            {{!Asttypes.override_flag.Override} [Override]} and [s] is [None],\n          - [inherit! CE as x] when [flag] is\n            {{!Asttypes.override_flag.Override} [Override]} and [s] is [Some x]\n       "]
    [@@@ocaml.text "{2 Class expr}"]
    val pcl_open :
      loc:Location.t -> open_description -> class_expr -> class_expr[@@ocaml.doc
                                                                    " [pcl_open] constructs an {! Ast.Pcl_open}\n\n{b Example OCaml}\n\n [let open M in CE]  "]
    val pcl_extension : loc:Location.t -> extension -> class_expr[@@ocaml.doc
                                                                   " [pcl_extension] constructs an {! Ast.Pcl_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pcl_constraint :
      loc:Location.t -> class_expr -> class_type -> class_expr[@@ocaml.doc
                                                                " [pcl_constraint] constructs an {! Ast.Pcl_constraint}\n\n{b Example OCaml}\n\n [(CE : CT)]  "]
    val pcl_let :
      loc:Location.t ->
        rec_flag -> value_binding list -> class_expr -> class_expr[@@ocaml.doc
                                                                    " [pcl_let] constructs an {! Ast.Pcl_let}\n\n{b Example OCaml}\n\n [Pcl_let(rec, [(P1, E1); ... ; (Pn, En)], CE)] represents:\n\n          - [let P1 = E1 and ... and Pn = EN in CE] when [rec] is\n            {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},\n          - [let rec P1 = E1 and ... and Pn = EN in CE] when [rec] is\n            {{!Asttypes.rec_flag.Recursive} [Recursive]}.  "]
    val pcl_apply :
      loc:Location.t ->
        class_expr -> (arg_label * expression) list -> class_expr[@@ocaml.doc
                                                                   " [pcl_apply] constructs an {! Ast.Pcl_apply}\n\n{b Example OCaml}\n\n [Pcl_apply(CE, [(l1,E1) ; ... ; (ln,En)])] represents\n          [CE ~l1:E1 ... ~ln:En]. [li] can be empty (non labeled argument) or\n          start with [?] (optional argument).\n\n          Invariant: [n > 0]  "]
    val pcl_fun :
      loc:Location.t ->
        arg_label -> expression option -> pattern -> class_expr -> class_expr
    [@@ocaml.doc
      " [pcl_fun] constructs an {! Ast.Pcl_fun}\n\n{b Example OCaml}\n\n [Pcl_fun(lbl, exp0, P, CE)] represents:\n\n          - [fun P -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Nolabel} [Nolabel]} and [exp0] is [None],\n          - [fun ~l:P -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled l]} and [exp0] is [None],\n          - [fun ?l:P -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is [None],\n          - [fun ?l:(P = E0) -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is\n            [Some E0].  "]
    val pcl_structure : loc:Location.t -> class_structure -> class_expr
    [@@ocaml.doc
      " [pcl_structure] constructs an {! Ast.Pcl_structure}\n\n{b Example OCaml}\n\n [object ... end]  "]
    val pcl_constr :
      loc:Location.t -> longident loc -> core_type list -> class_expr
    [@@ocaml.doc
      " [pcl_constr] constructs an {! Ast.Pcl_constr}\n\n{b Example OCaml}\n\n [c] and [['a1, ..., 'an] c]  "]
    [@@@ocaml.text "{2 Case}"]
    val case :
      lhs:pattern -> guard:expression option -> rhs:expression -> case
    [@@ocaml.doc
      " [case] constructs an {! Ast.case}\n\n{b Example OCaml}\n\n Values of type {!case} represents [(P -> E)] or [(P when E0 -> E)]  "]
    [@@@ocaml.text "{2 Binding op}"]
    val binding_op :
      loc:Location.t ->
        op:string loc -> pat:pattern -> exp:expression -> binding_op[@@ocaml.doc
                                                                    " [binding_op] constructs an {! Ast.binding_op}\n\n "]
    [@@@ocaml.text "{2 Attribute}"]
    val attribute :
      loc:Location.t -> name:string loc -> payload:payload -> attribute
    [@@ocaml.doc
      " [attribute] constructs an {! Ast.attribute}\n\n{b Example OCaml}\n\n Attributes such as [[@id ARG]] and [[@@id ARG]].\n\n    Metadata containers passed around within the AST. The compiler ignores\n    unknown attributes.  "]
    [@@@ocaml.text "{2 'a open infos}"]
    val open_infos :
      loc:Location.t -> expr:'a -> override:override_flag -> 'a open_infos
    [@@ocaml.doc
      " [open_infos] constructs an {! Ast.'a open_infos}\n\n{b Example OCaml}\n\n Values of type ['a open_infos] represents:\n\n    - [open! X] when {{!open_infos.popen_override} [popen_override]} is\n      {{!Asttypes.override_flag.Override} [Override]} (silences the \"used\n      identifier shadowing\" warning)\n    - [open  X] when {{!open_infos.popen_override} [popen_override]} is\n      {{!Asttypes.override_flag.Fresh} [Fresh]}  "]
    [@@@ocaml.text "{2 'a include infos}"]
    val include_infos : loc:Location.t -> 'a -> 'a include_infos[@@ocaml.doc
                                                                  " [include_infos] constructs an {! Ast.'a include_infos}\n\n "]
    [@@@ocaml.text "{2 'a class infos}"]
    val class_infos :
      loc:Location.t ->
        virt:virtual_flag ->
          params:(core_type * (variance * injectivity)) list ->
            name:string loc -> expr:'a -> 'a class_infos[@@ocaml.doc
                                                          " [class_infos] constructs an {! Ast.'a class_infos}\n\n{b Example OCaml}\n\n Values of type [class_expr class_infos] represents:\n\n    - [class c = ...]\n    - [class ['a1,...,'an] c = ...]\n    - [class virtual c = ...]\n\n    They are also used for \"class type\" declaration.  "]
  end
module type Intf_located  =
  sig
    val loc : Location.t
    [@@@ocaml.text "{2 Value description}"]
    val value_description :
      name:string loc ->
        type_:core_type -> prim:string list -> value_description[@@ocaml.doc
                                                                  " [value_description] constructs an {! Ast.value_description}\n\n{b Example OCaml}\n\n Values of type {!value_description} represents:\n\n    - [val x: T], when {{!value_description.pval_prim} [pval_prim]} is [[]]\n    - [external x: T = \"s1\" ... \"sn\"] when\n      {{!value_description.pval_prim} [pval_prim]} is [[\"s1\";...\"sn\"]]  "]
    [@@@ocaml.text "{2 Value binding}"]
    val value_binding : pat:pattern -> expr:expression -> value_binding
    [@@ocaml.doc " [value_binding] constructs an {! Ast.value_binding}\n\n "]
    [@@@ocaml.text "{2 Type extension}"]
    val type_extension :
      path:longident loc ->
        params:(core_type * (variance * injectivity)) list ->
          constructors:extension_constructor list ->
            private_:private_flag -> type_extension[@@ocaml.doc
                                                     " [type_extension] constructs an {! Ast.type_extension}\n\n{b Example OCaml}\n\n Definition of new extensions constructors for the extensive sum type [t]\n    ([type t += ...]).  "]
    [@@@ocaml.text "{2 Type exception}"]
    val type_exception : extension_constructor -> type_exception[@@ocaml.doc
                                                                  " [type_exception] constructs an {! Ast.type_exception}\n\n{b Example OCaml}\n\n Definition of a new exception ([exception E]).  "]
    [@@@ocaml.text "{2 Type declaration}"]
    val type_declaration :
      name:string loc ->
        params:(core_type * (variance * injectivity)) list ->
          cstrs:(core_type * core_type * location) list ->
            kind:type_kind ->
              private_:private_flag ->
                manifest:core_type option -> type_declaration[@@ocaml.doc
                                                               " [type_declaration] constructs an {! Ast.type_declaration}\n\n{b Example OCaml}\n\n Here are type declarations and their representation, for various\n    {{!type_declaration.ptype_kind} [ptype_kind]} and\n    {{!type_declaration.ptype_manifest} [ptype_manifest]} values:\n\n    - [type t] when [type_kind] is\n      {{!type_kind.Ptype_abstract} [Ptype_abstract]}, and [manifest] is [None],\n    - [type t = T0] when [type_kind] is\n      {{!type_kind.Ptype_abstract} [Ptype_abstract]}, and [manifest] is\n      [Some T0],\n    - [type t = C of T | ...] when [type_kind] is\n      {{!type_kind.Ptype_variant} [Ptype_variant]}, and [manifest] is [None],\n    - [type t = T0 = C of T | ...] when [type_kind] is\n      {{!type_kind.Ptype_variant} [Ptype_variant]}, and [manifest] is [Some T0],\n    - [type t = {l: T; ...}] when [type_kind] is\n      {{!type_kind.Ptype_record} [Ptype_record]}, and [manifest] is [None],\n    - [type t = T0 = {l : T; ...}] when [type_kind] is\n      {{!type_kind.Ptype_record} [Ptype_record]}, and [manifest] is [Some T0],\n    - [type t = ..] when [type_kind] is {{!type_kind.Ptype_open} [Ptype_open]},\n      and [manifest] is [None].  "]
    [@@@ocaml.text "{2 Toplevel directive}"]
    val toplevel_directive :
      name:string loc -> arg:directive_argument option -> toplevel_directive
    [@@ocaml.doc
      " [toplevel_directive] constructs an {! Ast.toplevel_directive}\n\n "]
    [@@@ocaml.text "{2 Structure item}"]
    val pstr_extension : extension -> attributes -> structure_item[@@ocaml.doc
                                                                    " [pstr_extension] constructs an {! Ast.Pstr_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val pstr_attribute : attribute -> structure_item[@@ocaml.doc
                                                      " [pstr_attribute] constructs an {! Ast.Pstr_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val pstr_include : include_declaration -> structure_item[@@ocaml.doc
                                                              " [pstr_include] constructs an {! Ast.Pstr_include}\n\n{b Example OCaml}\n\n [include ME]  "]
    val pstr_class_type : class_type_declaration list -> structure_item
    [@@ocaml.doc
      " [pstr_class_type] constructs an {! Ast.Pstr_class_type}\n\n{b Example OCaml}\n\n [class type ct1 = ... and ... and ctn = ...]  "]
    val pstr_class : class_declaration list -> structure_item[@@ocaml.doc
                                                               " [pstr_class] constructs an {! Ast.Pstr_class}\n\n{b Example OCaml}\n\n [class c1 = ... and ... and cn = ...]  "]
    val pstr_open : open_declaration -> structure_item[@@ocaml.doc
                                                        " [pstr_open] constructs an {! Ast.Pstr_open}\n\n{b Example OCaml}\n\n [open X]  "]
    val pstr_modtype : module_type_declaration -> structure_item[@@ocaml.doc
                                                                  " [pstr_modtype] constructs an {! Ast.Pstr_modtype}\n\n{b Example OCaml}\n\n [module type S = MT]  "]
    val pstr_recmodule : module_binding list -> structure_item[@@ocaml.doc
                                                                " [pstr_recmodule] constructs an {! Ast.Pstr_recmodule}\n\n{b Example OCaml}\n\n [module rec X1 = ME1 and ... and Xn = MEn]  "]
    val pstr_module : module_binding -> structure_item[@@ocaml.doc
                                                        " [pstr_module] constructs an {! Ast.Pstr_module}\n\n{b Example OCaml}\n\n [module X = ME]  "]
    val pstr_exception : type_exception -> structure_item[@@ocaml.doc
                                                           " [pstr_exception] constructs an {! Ast.Pstr_exception}\n\n{b Example OCaml}\n\n - [exception C of T]\n          - [exception C = M.X]  "]
    val pstr_typext : type_extension -> structure_item[@@ocaml.doc
                                                        " [pstr_typext] constructs an {! Ast.Pstr_typext}\n\n{b Example OCaml}\n\n [type t1 += ...]  "]
    val pstr_type : rec_flag -> type_declaration list -> structure_item
    [@@ocaml.doc
      " [pstr_type] constructs an {! Ast.Pstr_type}\n\n{b Example OCaml}\n\n [type t1 = ... and ... and tn = ...]  "]
    val pstr_primitive : value_description -> structure_item[@@ocaml.doc
                                                              " [pstr_primitive] constructs an {! Ast.Pstr_primitive}\n\n{b Example OCaml}\n\n - [val x: T]\n          - [external x: T = \"s1\" ... \"sn\" ] "]
    val pstr_value : rec_flag -> value_binding list -> structure_item
    [@@ocaml.doc
      " [pstr_value] constructs an {! Ast.Pstr_value}\n\n{b Example OCaml}\n\n [Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:\n\n          - [let P1 = E1 and ... and Pn = EN] when [rec] is\n            {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},\n          - [let rec P1 = E1 and ... and Pn = EN ] when [rec] is\n            {{!Asttypes.rec_flag.Recursive} [Recursive]}.  "]
    val pstr_eval : expression -> attributes -> structure_item[@@ocaml.doc
                                                                " [pstr_eval] constructs an {! Ast.Pstr_eval}\n\n{b Example OCaml}\n\n [E]  "]
    [@@@ocaml.text "{2 Signature item}"]
    val psig_extension : extension -> attributes -> signature_item[@@ocaml.doc
                                                                    " [psig_extension] constructs an {! Ast.Psig_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val psig_attribute : attribute -> signature_item[@@ocaml.doc
                                                      " [psig_attribute] constructs an {! Ast.Psig_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val psig_class_type : class_type_declaration list -> signature_item
    [@@ocaml.doc
      " [psig_class_type] constructs an {! Ast.Psig_class_type}\n\n{b Example OCaml}\n\n [class type ct1 = ... and ... and ctn = ...]  "]
    val psig_class : class_description list -> signature_item[@@ocaml.doc
                                                               " [psig_class] constructs an {! Ast.Psig_class}\n\n{b Example OCaml}\n\n [class c1 : ... and ... and cn : ...]  "]
    val psig_include : include_description -> signature_item[@@ocaml.doc
                                                              " [psig_include] constructs an {! Ast.Psig_include}\n\n{b Example OCaml}\n\n [include MT]  "]
    val psig_open : open_description -> signature_item[@@ocaml.doc
                                                        " [psig_open] constructs an {! Ast.Psig_open}\n\n{b Example OCaml}\n\n [open X]  "]
    val psig_modtypesubst : module_type_declaration -> signature_item
    [@@ocaml.doc
      " [psig_modtypesubst] constructs an {! Ast.Psig_modtypesubst}\n\n{b Example OCaml}\n\n [module type S :=  ...]  "]
    val psig_modtype : module_type_declaration -> signature_item[@@ocaml.doc
                                                                  " [psig_modtype] constructs an {! Ast.Psig_modtype}\n\n{b Example OCaml}\n\n [module type S = MT] and [module type S]  "]
    val psig_recmodule : module_declaration list -> signature_item[@@ocaml.doc
                                                                    " [psig_recmodule] constructs an {! Ast.Psig_recmodule}\n\n{b Example OCaml}\n\n [module rec X1 : MT1 and ... and Xn : MTn]  "]
    val psig_modsubst : module_substitution -> signature_item[@@ocaml.doc
                                                               " [psig_modsubst] constructs an {! Ast.Psig_modsubst}\n\n{b Example OCaml}\n\n [module X := M]  "]
    val psig_module : module_declaration -> signature_item[@@ocaml.doc
                                                            " [psig_module] constructs an {! Ast.Psig_module}\n\n{b Example OCaml}\n\n [module X = M] and [module X : MT]  "]
    val psig_exception : type_exception -> signature_item[@@ocaml.doc
                                                           " [psig_exception] constructs an {! Ast.Psig_exception}\n\n{b Example OCaml}\n\n [exception C of T]  "]
    val psig_typext : type_extension -> signature_item[@@ocaml.doc
                                                        " [psig_typext] constructs an {! Ast.Psig_typext}\n\n{b Example OCaml}\n\n [type t1 += ...]  "]
    val psig_typesubst : type_declaration list -> signature_item[@@ocaml.doc
                                                                  " [psig_typesubst] constructs an {! Ast.Psig_typesubst}\n\n{b Example OCaml}\n\n [type t1 := ... and ... and tn := ...]  "]
    val psig_type : rec_flag -> type_declaration list -> signature_item
    [@@ocaml.doc
      " [psig_type] constructs an {! Ast.Psig_type}\n\n{b Example OCaml}\n\n [type t1 = ... and ... and tn  = ...]  "]
    val psig_value : value_description -> signature_item[@@ocaml.doc
                                                          " [psig_value] constructs an {! Ast.Psig_value}\n\n{b Example OCaml}\n\n - [val x: T]\n          - [external x: T = \"s1\" ... \"sn\"]  "]
    [@@@ocaml.text "{2 Row field}"]
    val rinherit : core_type -> row_field[@@ocaml.doc
                                           " [rinherit] constructs an {! Ast.Rinherit}\n\n{b Example OCaml}\n\n [[ | t ]]  "]
    val rtag : label loc -> bool -> core_type list -> row_field[@@ocaml.doc
                                                                 " [rtag] constructs an {! Ast.Rtag}\n\n{b Example OCaml}\n\n [Rtag(`A, b, l)] represents:\n\n          - [`A] when [b] is [true] and [l] is [[]],\n          - [`A of T] when [b] is [false] and [l] is [[T]],\n          - [`A of T1 & .. & Tn] when [b] is [false] and [l] is [[T1;...Tn]],\n          - [`A of & T1 & .. & Tn] when [b] is [true] and [l] is [[T1;...Tn]].\n\n          - The [bool] field is true if the tag contains a constant (empty)\n            constructor.\n          - [&] occurs when several types are used for the same constructor (see\n            4.2 in the manual)  "]
    [@@@ocaml.text "{2 Position}"]
    val position :
      fname:string -> lnum:int -> bol:int -> cnum:int -> position[@@ocaml.doc
                                                                   " [position] constructs an {! Ast.position}\n\n "]
    [@@@ocaml.text "{2 Pattern}"]
    val ppat_open : longident loc -> pattern -> pattern[@@ocaml.doc
                                                         " [ppat_open] constructs an {! Ast.Ppat_open}\n\n{b Example OCaml}\n\n Pattern [M.(P)]  "]
    val ppat_extension : extension -> pattern[@@ocaml.doc
                                               " [ppat_extension] constructs an {! Ast.Ppat_extension}\n\n{b Example OCaml}\n\n Pattern [[%id]]  "]
    val ppat_exception : pattern -> pattern[@@ocaml.doc
                                             " [ppat_exception] constructs an {! Ast.Ppat_exception}\n\n{b Example OCaml}\n\n Pattern [exception P]  "]
    val ppat_unpack : string option loc -> pattern[@@ocaml.doc
                                                    " [ppat_unpack] constructs an {! Ast.Ppat_unpack}\n\n{b Example OCaml}\n\n [Ppat_unpack(s)] represents:\n\n          - [(module P)] when [s] is [Some \"P\"]\n          - [(module _)] when [s] is [None]\n\n          Note: [(module P : S)] is represented as\n          [Ppat_constraint(Ppat_unpack(Some \"P\"), Ptyp_package S)]  "]
    val ppat_lazy : pattern -> pattern[@@ocaml.doc
                                        " [ppat_lazy] constructs an {! Ast.Ppat_lazy}\n\n{b Example OCaml}\n\n Pattern [lazy P]  "]
    val ppat_type : longident loc -> pattern[@@ocaml.doc
                                              " [ppat_type] constructs an {! Ast.Ppat_type}\n\n{b Example OCaml}\n\n Pattern [#tconst]  "]
    val ppat_constraint : pattern -> core_type -> pattern[@@ocaml.doc
                                                           " [ppat_constraint] constructs an {! Ast.Ppat_constraint}\n\n{b Example OCaml}\n\n Pattern [(P : T)]  "]
    val ppat_or : pattern -> pattern -> pattern[@@ocaml.doc
                                                 " [ppat_or] constructs an {! Ast.Ppat_or}\n\n{b Example OCaml}\n\n Pattern [P1 | P2]  "]
    val ppat_array : pattern list -> pattern[@@ocaml.doc
                                              " [ppat_array] constructs an {! Ast.Ppat_array}\n\n{b Example OCaml}\n\n Pattern [[| P1; ...; Pn |]]  "]
    val ppat_record :
      (longident loc * pattern) list -> closed_flag -> pattern[@@ocaml.doc
                                                                " [ppat_record] constructs an {! Ast.Ppat_record}\n\n{b Example OCaml}\n\n [Ppat_record([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:\n\n          - [{ l1=P1; ...; ln=Pn }] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}\n          - [{ l1=P1; ...; ln=Pn; _}] when [flag] is\n            {{!Asttypes.closed_flag.Open} [Open]}\n\n          Invariant: [n > 0]  "]
    val ppat_variant : label -> pattern option -> pattern[@@ocaml.doc
                                                           " [ppat_variant] constructs an {! Ast.Ppat_variant}\n\n{b Example OCaml}\n\n [Ppat_variant(`A, pat)] represents:\n\n          - [`A] when [pat] is [None],\n          - [`A P] when [pat] is [Some P]  "]
    val ppat_construct :
      longident loc -> (string loc list * pattern) option -> pattern[@@ocaml.doc
                                                                    " [ppat_construct] constructs an {! Ast.Ppat_construct}\n\n{b Example OCaml}\n\n [Ppat_construct(C, args)] represents:\n\n          - [C] when [args] is [None],\n          - [C P] when [args] is [Some ([], P)]\n          - [C (P1, ..., Pn)] when [args] is\n            [Some ([], Ppat_tuple [P1; ...; Pn])]\n          - [C (type a b) P] when [args] is [Some ([a; b], P)]  "]
    val ppat_tuple : pattern list -> pattern[@@ocaml.doc
                                              " [ppat_tuple] constructs an {! Ast.Ppat_tuple}\n\n{b Example OCaml}\n\n Patterns [(P1, ..., Pn)].\n\n          Invariant: [n >= 2]  "]
    val ppat_interval : constant -> constant -> pattern[@@ocaml.doc
                                                         " [ppat_interval] constructs an {! Ast.Ppat_interval}\n\n{b Example OCaml}\n\n Patterns such as ['a'..'z'].\n\n          Other forms of interval are recognized by the parser but rejected by\n          the type-checker.  "]
    val ppat_constant : constant -> pattern[@@ocaml.doc
                                             " [ppat_constant] constructs an {! Ast.Ppat_constant}\n\n{b Example OCaml}\n\n Patterns such as [1], ['a'], [\"true\"], [1.0], [1l], [1L], [1n]  "]
    val ppat_alias : pattern -> string loc -> pattern[@@ocaml.doc
                                                       " [ppat_alias] constructs an {! Ast.Ppat_alias}\n\n{b Example OCaml}\n\n An alias pattern such as [P as 'a]  "]
    val ppat_var : string loc -> pattern[@@ocaml.doc
                                          " [ppat_var] constructs an {! Ast.Ppat_var}\n\n{b Example OCaml}\n\n A variable pattern such as [x]  "]
    val ppat_any : pattern[@@ocaml.doc
                            " [ppat_any] constructs an {! Ast.Ppat_any}\n\n{b Example OCaml}\n\n The pattern [_].  "]
    [@@@ocaml.text "{2 Object field}"]
    val oinherit : core_type -> object_field[@@ocaml.doc
                                              " [oinherit] constructs an {! Ast.Oinherit}\n\n "]
    val otag : label loc -> core_type -> object_field[@@ocaml.doc
                                                       " [otag] constructs an {! Ast.Otag}\n\n "]
    [@@@ocaml.text "{2 Module type declaration}"]
    val module_type_declaration :
      name:string loc -> type_:module_type option -> module_type_declaration
    [@@ocaml.doc
      " [module_type_declaration] constructs an {! Ast.module_type_declaration}\n\n{b Example OCaml}\n\n Values of type [module_type_declaration] represents:\n\n    - [S = MT],\n    - [S] for abstract module type declaration, when\n      {{!module_type_declaration.pmtd_type} [pmtd_type]} is [None].  "]
    [@@@ocaml.text "{2 Module type}"]
    val pmty_alias : longident loc -> module_type[@@ocaml.doc
                                                   " [pmty_alias] constructs an {! Ast.Pmty_alias}\n\n{b Example OCaml}\n\n [(module M)]  "]
    val pmty_extension : extension -> module_type[@@ocaml.doc
                                                   " [pmty_extension] constructs an {! Ast.Pmty_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pmty_typeof : module_expr -> module_type[@@ocaml.doc
                                                  " [pmty_typeof] constructs an {! Ast.Pmty_typeof}\n\n{b Example OCaml}\n\n [module type of ME]  "]
    val pmty_with : module_type -> with_constraint list -> module_type
    [@@ocaml.doc
      " [pmty_with] constructs an {! Ast.Pmty_with}\n\n{b Example OCaml}\n\n [MT with ...]  "]
    val pmty_functor : functor_parameter -> module_type -> module_type
    [@@ocaml.doc
      " [pmty_functor] constructs an {! Ast.Pmty_functor}\n\n{b Example OCaml}\n\n [functor(X : MT1) -> MT2]  "]
    val pmty_signature : signature -> module_type[@@ocaml.doc
                                                   " [pmty_signature] constructs an {! Ast.Pmty_signature}\n\n{b Example OCaml}\n\n [sig ... end]  "]
    val pmty_ident : longident loc -> module_type[@@ocaml.doc
                                                   " [pmty_ident] constructs an {! Ast.Pmty_ident}\n\n{b Example OCaml}\n\n [Pmty_ident(S)] represents [S]  "]
    [@@@ocaml.text "{2 Module substitution}"]
    val module_substitution :
      name:string loc -> manifest:longident loc -> module_substitution
    [@@ocaml.doc
      " [module_substitution] constructs an {! Ast.module_substitution}\n\n{b Example OCaml}\n\n Values of type [module_substitution] represents [S := M]  "]
    [@@@ocaml.text "{2 Module expr}"]
    val pmod_extension : extension -> module_expr[@@ocaml.doc
                                                   " [pmod_extension] constructs an {! Ast.Pmod_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pmod_unpack : expression -> module_expr[@@ocaml.doc
                                                 " [pmod_unpack] constructs an {! Ast.Pmod_unpack}\n\n{b Example OCaml}\n\n [(val E)]  "]
    val pmod_constraint : module_expr -> module_type -> module_expr[@@ocaml.doc
                                                                    " [pmod_constraint] constructs an {! Ast.Pmod_constraint}\n\n{b Example OCaml}\n\n [(ME : MT)]  "]
    val pmod_apply : module_expr -> module_expr -> module_expr[@@ocaml.doc
                                                                " [pmod_apply] constructs an {! Ast.Pmod_apply}\n\n{b Example OCaml}\n\n [ME1(ME2)]  "]
    val pmod_functor : functor_parameter -> module_expr -> module_expr
    [@@ocaml.doc
      " [pmod_functor] constructs an {! Ast.Pmod_functor}\n\n{b Example OCaml}\n\n [functor(X : MT1) -> ME]  "]
    val pmod_structure : structure -> module_expr[@@ocaml.doc
                                                   " [pmod_structure] constructs an {! Ast.Pmod_structure}\n\n{b Example OCaml}\n\n [struct ... end]  "]
    val pmod_ident : longident loc -> module_expr[@@ocaml.doc
                                                   " [pmod_ident] constructs an {! Ast.Pmod_ident}\n\n{b Example OCaml}\n\n [X]  "]
    [@@@ocaml.text "{2 Module declaration}"]
    val module_declaration :
      name:string option loc -> type_:module_type -> module_declaration
    [@@ocaml.doc
      " [module_declaration] constructs an {! Ast.module_declaration}\n\n{b Example OCaml}\n\n Values of type [module_declaration] represents [S : MT]  "]
    [@@@ocaml.text "{2 Module binding}"]
    val module_binding :
      name:string option loc -> expr:module_expr -> module_binding[@@ocaml.doc
                                                                    " [module_binding] constructs an {! Ast.module_binding}\n\n{b Example OCaml}\n\n Values of type [module_binding] represents [module X = ME]  "]
    [@@@ocaml.text "{2 Location}"]
    val location : start:position -> end_:position -> ghost:bool -> location
    [@@ocaml.doc " [location] constructs an {! Ast.location}\n\n "]
    [@@@ocaml.text "{2 Letop}"]
    val letop :
      let_:binding_op -> ands:binding_op list -> body:expression -> letop
    [@@ocaml.doc " [letop] constructs an {! Ast.letop}\n\n "]
    [@@@ocaml.text "{2 Label declaration}"]
    val label_declaration :
      name:string loc ->
        mutable_:mutable_flag -> type_:core_type -> label_declaration
    [@@ocaml.doc
      " [label_declaration] constructs an {! Ast.label_declaration}\n\n{b Example OCaml}\n\n - [{ ...; l: T; ... }] when {{!label_declaration.pld_mutable} [pld_mutable]}\n      is {{!Asttypes.mutable_flag.Immutable} [Immutable]},\n    - [{ ...; mutable l: T; ... }] when\n      {{!label_declaration.pld_mutable} [pld_mutable]} is\n      {{!Asttypes.mutable_flag.Mutable} [Mutable]}.\n\n    Note: [T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}.  "]
    [@@@ocaml.text "{2 Extension constructor}"]
    val extension_constructor :
      name:string loc ->
        kind:extension_constructor_kind -> extension_constructor[@@ocaml.doc
                                                                  " [extension_constructor] constructs an {! Ast.extension_constructor}\n\n "]
    [@@@ocaml.text "{2 Expression}"]
    val pexp_unreachable : expression[@@ocaml.doc
                                       " [pexp_unreachable] constructs an {! Ast.Pexp_unreachable}\n\n{b Example OCaml}\n\n [.]  "]
    val pexp_extension : extension -> expression[@@ocaml.doc
                                                  " [pexp_extension] constructs an {! Ast.Pexp_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pexp_letop : letop -> expression[@@ocaml.doc
                                          " [pexp_letop] constructs an {! Ast.Pexp_letop}\n\n{b Example OCaml}\n\n - [let* P = E0 in E1]\n          - [let* P0 = E00 and* P1 = E01 in E1]  "]
    val pexp_open : open_declaration -> expression -> expression[@@ocaml.doc
                                                                  " [pexp_open] constructs an {! Ast.Pexp_open}\n\n{b Example OCaml}\n\n - [M.(E)]\n          - [let open M in E]\n          - [let open! M in E]  "]
    val pexp_pack : module_expr -> expression[@@ocaml.doc
                                               " [pexp_pack] constructs an {! Ast.Pexp_pack}\n\n{b Example OCaml}\n\n [(module ME)].\n\n          [(module ME : S)] is represented as\n          [Pexp_constraint(Pexp_pack ME, Ptyp_package S)]  "]
    val pexp_newtype : string loc -> expression -> expression[@@ocaml.doc
                                                               " [pexp_newtype] constructs an {! Ast.Pexp_newtype}\n\n{b Example OCaml}\n\n [fun (type t) -> E]  "]
    val pexp_object : class_structure -> expression[@@ocaml.doc
                                                     " [pexp_object] constructs an {! Ast.Pexp_object}\n\n{b Example OCaml}\n\n [object ... end]  "]
    val pexp_poly : expression -> core_type option -> expression[@@ocaml.doc
                                                                  " [pexp_poly] constructs an {! Ast.Pexp_poly}\n\n{b Example OCaml}\n\n Used for method bodies.\n\n          Can only be used as the expression under\n          {{!class_field_kind.Cfk_concrete} [Cfk_concrete]} for methods (not\n          values).  "]
    val pexp_lazy : expression -> expression[@@ocaml.doc
                                              " [pexp_lazy] constructs an {! Ast.Pexp_lazy}\n\n{b Example OCaml}\n\n [lazy E]  "]
    val pexp_assert : expression -> expression[@@ocaml.doc
                                                " [pexp_assert] constructs an {! Ast.Pexp_assert}\n\n{b Example OCaml}\n\n [assert E].\n\n          Note: [assert false] is treated in a special way by the type-checker.\n       "]
    val pexp_letexception : extension_constructor -> expression -> expression
    [@@ocaml.doc
      " [pexp_letexception] constructs an {! Ast.Pexp_letexception}\n\n{b Example OCaml}\n\n [let exception C in E]  "]
    val pexp_letmodule :
      string option loc -> module_expr -> expression -> expression[@@ocaml.doc
                                                                    " [pexp_letmodule] constructs an {! Ast.Pexp_letmodule}\n\n{b Example OCaml}\n\n [let module M = ME in E]  "]
    val pexp_override : (label loc * expression) list -> expression[@@ocaml.doc
                                                                    " [pexp_override] constructs an {! Ast.Pexp_override}\n\n{b Example OCaml}\n\n [{< x1 = E1; ...; xn = En >}]  "]
    val pexp_setinstvar : label loc -> expression -> expression[@@ocaml.doc
                                                                 " [pexp_setinstvar] constructs an {! Ast.Pexp_setinstvar}\n\n{b Example OCaml}\n\n [x <- 2]  "]
    val pexp_new : longident loc -> expression[@@ocaml.doc
                                                " [pexp_new] constructs an {! Ast.Pexp_new}\n\n{b Example OCaml}\n\n [new M.c]  "]
    val pexp_send : expression -> label loc -> expression[@@ocaml.doc
                                                           " [pexp_send] constructs an {! Ast.Pexp_send}\n\n{b Example OCaml}\n\n [E # m]  "]
    val pexp_coerce :
      expression -> core_type option -> core_type -> expression[@@ocaml.doc
                                                                 " [pexp_coerce] constructs an {! Ast.Pexp_coerce}\n\n{b Example OCaml}\n\n [Pexp_coerce(E, from, T)] represents\n\n          - [(E :> T)] when [from] is [None],\n          - [(E : T0 :> T)] when [from] is [Some T0].  "]
    val pexp_constraint : expression -> core_type -> expression[@@ocaml.doc
                                                                 " [pexp_constraint] constructs an {! Ast.Pexp_constraint}\n\n{b Example OCaml}\n\n [(E : T)]  "]
    val pexp_for :
      pattern ->
        expression ->
          expression -> direction_flag -> expression -> expression[@@ocaml.doc
                                                                    " [pexp_for] constructs an {! Ast.Pexp_for}\n\n{b Example OCaml}\n\n [Pexp_for(i, E1, E2, direction, E3)] represents:\n\n          - [for i = E1 to E2 do E3 done] when [direction] is\n            {{!Asttypes.direction_flag.Upto} [Upto]}\n          - [for i = E1 downto E2 do E3 done] when [direction] is\n            {{!Asttypes.direction_flag.Downto} [Downto]}  "]
    val pexp_while : expression -> expression -> expression[@@ocaml.doc
                                                             " [pexp_while] constructs an {! Ast.Pexp_while}\n\n{b Example OCaml}\n\n [while E1 do E2 done]  "]
    val pexp_sequence : expression -> expression -> expression[@@ocaml.doc
                                                                " [pexp_sequence] constructs an {! Ast.Pexp_sequence}\n\n{b Example OCaml}\n\n [E1; E2]  "]
    val pexp_ifthenelse :
      expression -> expression -> expression option -> expression[@@ocaml.doc
                                                                   " [pexp_ifthenelse] constructs an {! Ast.Pexp_ifthenelse}\n\n{b Example OCaml}\n\n [if E1 then E2 else E3]  "]
    val pexp_array : expression list -> expression[@@ocaml.doc
                                                    " [pexp_array] constructs an {! Ast.Pexp_array}\n\n{b Example OCaml}\n\n [[| E1; ...; En |]]  "]
    val pexp_setfield :
      expression -> longident loc -> expression -> expression[@@ocaml.doc
                                                               " [pexp_setfield] constructs an {! Ast.Pexp_setfield}\n\n{b Example OCaml}\n\n [E1.l <- E2]  "]
    val pexp_field : expression -> longident loc -> expression[@@ocaml.doc
                                                                " [pexp_field] constructs an {! Ast.Pexp_field}\n\n{b Example OCaml}\n\n [E.l]  "]
    val pexp_record :
      (longident loc * expression) list -> expression option -> expression
    [@@ocaml.doc
      " [pexp_record] constructs an {! Ast.Pexp_record}\n\n{b Example OCaml}\n\n [Pexp_record([(l1,P1) ; ... ; (ln,Pn)], exp0)] represents\n\n          - [{ l1=P1; ...; ln=Pn }] when [exp0] is [None]\n          - [{ E0 with l1=P1; ...; ln=Pn }] when [exp0] is [Some E0]\n\n          Invariant: [n > 0]  "]
    val pexp_variant : label -> expression option -> expression[@@ocaml.doc
                                                                 " [pexp_variant] constructs an {! Ast.Pexp_variant}\n\n{b Example OCaml}\n\n [Pexp_variant(`A, exp)] represents\n\n          - [`A] when [exp] is [None]\n          - [`A E] when [exp] is [Some E]  "]
    val pexp_construct : longident loc -> expression option -> expression
    [@@ocaml.doc
      " [pexp_construct] constructs an {! Ast.Pexp_construct}\n\n{b Example OCaml}\n\n [Pexp_construct(C, exp)] represents:\n\n          - [C] when [exp] is [None],\n          - [C E] when [exp] is [Some E],\n          - [C (E1, ..., En)] when [exp] is [Some (Pexp_tuple[E1;...;En])]  "]
    val pexp_tuple : expression list -> expression[@@ocaml.doc
                                                    " [pexp_tuple] constructs an {! Ast.Pexp_tuple}\n\n{b Example OCaml}\n\n Expressions [(E1, ..., En)]\n\n          Invariant: [n >= 2]  "]
    val pexp_try : expression -> cases -> expression[@@ocaml.doc
                                                      " [pexp_try] constructs an {! Ast.Pexp_try}\n\n{b Example OCaml}\n\n [try E0 with P1 -> E1 | ... | Pn -> En]  "]
    val pexp_match : expression -> cases -> expression[@@ocaml.doc
                                                        " [pexp_match] constructs an {! Ast.Pexp_match}\n\n{b Example OCaml}\n\n [match E0 with P1 -> E1 | ... | Pn -> En]  "]
    val pexp_apply :
      expression -> (arg_label * expression) list -> expression[@@ocaml.doc
                                                                 " [pexp_apply] constructs an {! Ast.Pexp_apply}\n\n{b Example OCaml}\n\n [Pexp_apply(E0, [(l1, E1) ; ... ; (ln, En)])] represents\n          [E0 ~l1:E1 ... ~ln:En]\n\n          [li] can be {{!Asttypes.arg_label.Nolabel} [Nolabel]} (non labeled\n          argument), {{!Asttypes.arg_label.Labelled} [Labelled]} (labelled\n          arguments) or {{!Asttypes.arg_label.Optional} [Optional]} (optional\n          argument).\n\n          Invariant: [n > 0]  "]
    val pexp_fun :
      arg_label -> expression option -> pattern -> expression -> expression
    [@@ocaml.doc
      " [pexp_fun] constructs an {! Ast.Pexp_fun}\n\n{b Example OCaml}\n\n [Pexp_fun(lbl, exp0, P, E1)] represents:\n\n          - [fun P -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Nolabel} [Nolabel]} and [exp0] is [None]\n          - [fun ~l:P -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled l]} and [exp0] is [None]\n          - [fun ?l:P -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is [None]\n          - [fun ?l:(P = E0) -> E1] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is\n            [Some E0]\n\n          Notes:\n\n          - If [E0] is provided, only\n            {{!Asttypes.arg_label.Optional} [Optional]} is allowed.\n          - [fun P1 P2 .. Pn -> E1] is represented as nested\n            {{!expression_desc.Pexp_fun} [Pexp_fun]}.\n          - [let f P = E] is represented using\n            {{!expression_desc.Pexp_fun} [Pexp_fun]}.  "]
    val pexp_function : cases -> expression[@@ocaml.doc
                                             " [pexp_function] constructs an {! Ast.Pexp_function}\n\n{b Example OCaml}\n\n [function P1 -> E1 | ... | Pn -> En]  "]
    val pexp_let : rec_flag -> value_binding list -> expression -> expression
    [@@ocaml.doc
      " [pexp_let] constructs an {! Ast.Pexp_let}\n\n{b Example OCaml}\n\n [Pexp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:\n\n          - [let P1 = E1 and ... and Pn = EN in E] when [flag] is\n            {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},\n          - [let rec P1 = E1 and ... and Pn = EN in E] when [flag] is\n            {{!Asttypes.rec_flag.Recursive} [Recursive]}.  "]
    val pexp_constant : constant -> expression[@@ocaml.doc
                                                " [pexp_constant] constructs an {! Ast.Pexp_constant}\n\n{b Example OCaml}\n\n Expressions constant such as [1], ['a'], [\"true\"], [1.0], [1l], [1L],\n          [1n]  "]
    val pexp_ident : longident loc -> expression[@@ocaml.doc
                                                  " [pexp_ident] constructs an {! Ast.Pexp_ident}\n\n{b Example OCaml}\n\n Identifiers such as [x] and [M.x]  "]
    [@@@ocaml.text "{2 Directive argument}"]
    val pdir_bool : bool -> directive_argument[@@ocaml.doc
                                                " [pdir_bool] constructs an {! Ast.Pdir_bool}\n\n "]
    val pdir_ident : longident -> directive_argument[@@ocaml.doc
                                                      " [pdir_ident] constructs an {! Ast.Pdir_ident}\n\n "]
    val pdir_int : string -> char option -> directive_argument[@@ocaml.doc
                                                                " [pdir_int] constructs an {! Ast.Pdir_int}\n\n "]
    val pdir_string : string -> directive_argument[@@ocaml.doc
                                                    " [pdir_string] constructs an {! Ast.Pdir_string}\n\n "]
    [@@@ocaml.text "{2 Core type}"]
    val ptyp_extension : extension -> core_type[@@ocaml.doc
                                                 " [ptyp_extension] constructs an {! Ast.Ptyp_extension}\n\n{b Example OCaml}\n\n [[%id]].  "]
    val ptyp_package : package_type -> core_type[@@ocaml.doc
                                                  " [ptyp_package] constructs an {! Ast.Ptyp_package}\n\n{b Example OCaml}\n\n [(module S)].  "]
    val ptyp_poly : string loc list -> core_type -> core_type[@@ocaml.doc
                                                               " [ptyp_poly] constructs an {! Ast.Ptyp_poly}\n\n{b Example OCaml}\n\n ['a1 ... 'an. T]\n\n          Can only appear in the following context:\n\n          - As the {!core_type} of a\n            {{!pattern_desc.Ppat_constraint} [Ppat_constraint]} node\n            corresponding to a constraint on a let-binding:\n\n          {[\n            let x : 'a1 ... 'an. T = e ...\n          ]}\n          - Under {{!class_field_kind.Cfk_virtual} [Cfk_virtual]} for methods\n            (not values).\n\n          - As the {!core_type} of a\n            {{!class_type_field_desc.Pctf_method} [Pctf_method]} node.\n\n          - As the {!core_type} of a {{!expression_desc.Pexp_poly} [Pexp_poly]}\n            node.\n\n          - As the {{!label_declaration.pld_type} [pld_type]} field of a\n            {!label_declaration}.\n\n          - As a {!core_type} of a {{!core_type_desc.Ptyp_object} [Ptyp_object]}\n            node.\n\n          - As the {{!value_description.pval_type} [pval_type]} field of a\n            {!value_description}.  "]
    val ptyp_variant :
      row_field list -> closed_flag -> label list option -> core_type
    [@@ocaml.doc
      " [ptyp_variant] constructs an {! Ast.Ptyp_variant}\n\n{b Example OCaml}\n\n [Ptyp_variant([`A;`B], flag, labels)] represents:\n\n          - [[ `A|`B ]] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels] is [None],\n          - [[> `A|`B ]] when [flag] is {{!Asttypes.closed_flag.Open} [Open]},\n            and [labels] is [None],\n          - [[< `A|`B ]] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels] is\n            [Some []],\n          - [[< `A|`B > `X `Y ]] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]}, and [labels] is\n            [Some [\"X\";\"Y\"]].  "]
    val ptyp_alias : core_type -> string -> core_type[@@ocaml.doc
                                                       " [ptyp_alias] constructs an {! Ast.Ptyp_alias}\n\n{b Example OCaml}\n\n [T as 'a].  "]
    val ptyp_class : longident loc -> core_type list -> core_type[@@ocaml.doc
                                                                   " [ptyp_class] constructs an {! Ast.Ptyp_class}\n\n{b Example OCaml}\n\n [Ptyp_class(tconstr, l)] represents:\n\n          - [#tconstr] when [l=[]],\n          - [T #tconstr] when [l=[T]],\n          - [(T1, ..., Tn) #tconstr] when [l=[T1 ; ... ; Tn]].  "]
    val ptyp_object : object_field list -> closed_flag -> core_type[@@ocaml.doc
                                                                    " [ptyp_object] constructs an {! Ast.Ptyp_object}\n\n{b Example OCaml}\n\n [Ptyp_object([ l1:T1; ...; ln:Tn ], flag)] represents:\n\n          - [< l1:T1; ...; ln:Tn >] when [flag] is\n            {{!Asttypes.closed_flag.Closed} [Closed]},\n          - [< l1:T1; ...; ln:Tn; .. >] when [flag] is\n            {{!Asttypes.closed_flag.Open} [Open]}.  "]
    val ptyp_constr : longident loc -> core_type list -> core_type[@@ocaml.doc
                                                                    " [ptyp_constr] constructs an {! Ast.Ptyp_constr}\n\n{b Example OCaml}\n\n [Ptyp_constr(lident, l)] represents:\n\n          - [tconstr] when [l=[]],\n          - [T tconstr] when [l=[T]],\n          - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].  "]
    val ptyp_tuple : core_type list -> core_type[@@ocaml.doc
                                                  " [ptyp_tuple] constructs an {! Ast.Ptyp_tuple}\n\n{b Example OCaml}\n\n [Ptyp_tuple([T1 ; ... ; Tn])] represents a product type\n          [T1 * ... * Tn].\n\n          Invariant: [n >= 2].  "]
    val ptyp_arrow : arg_label -> core_type -> core_type -> core_type
    [@@ocaml.doc
      " [ptyp_arrow] constructs an {! Ast.Ptyp_arrow}\n\n{b Example OCaml}\n\n [Ptyp_arrow(lbl, T1, T2)] represents:\n\n          - [T1 -> T2] when [lbl] is {{!Asttypes.arg_label.Nolabel} [Nolabel]},\n          - [~l:T1 -> T2] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled]},\n          - [?l:T1 -> T2] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional]}.  "]
    val ptyp_var : string -> core_type[@@ocaml.doc
                                        " [ptyp_var] constructs an {! Ast.Ptyp_var}\n\n{b Example OCaml}\n\n A type variable such as ['a]  "]
    val ptyp_any : core_type[@@ocaml.doc
                              " [ptyp_any] constructs an {! Ast.Ptyp_any}\n\n{b Example OCaml}\n\n [_]  "]
    [@@@ocaml.text "{2 Constructor declaration}"]
    val constructor_declaration :
      name:string loc ->
        vars:string loc list ->
          args:constructor_arguments ->
            res:core_type option -> constructor_declaration[@@ocaml.doc
                                                             " [constructor_declaration] constructs an {! Ast.constructor_declaration}\n\n "]
    [@@@ocaml.text "{2 Class type field}"]
    val pctf_extension : extension -> class_type_field[@@ocaml.doc
                                                        " [pctf_extension] constructs an {! Ast.Pctf_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val pctf_attribute : attribute -> class_type_field[@@ocaml.doc
                                                        " [pctf_attribute] constructs an {! Ast.Pctf_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val pctf_constraint : (core_type * core_type) -> class_type_field
    [@@ocaml.doc
      " [pctf_constraint] constructs an {! Ast.Pctf_constraint}\n\n{b Example OCaml}\n\n [constraint T1 = T2]  "]
    val pctf_method :
      (label loc * private_flag * virtual_flag * core_type) ->
        class_type_field[@@ocaml.doc
                          " [pctf_method] constructs an {! Ast.Pctf_method}\n\n{b Example OCaml}\n\n [method x: T]\n\n          Note: [T] can be a {{!core_type_desc.Ptyp_poly} [Ptyp_poly]}.  "]
    val pctf_val :
      (label loc * mutable_flag * virtual_flag * core_type) ->
        class_type_field[@@ocaml.doc
                          " [pctf_val] constructs an {! Ast.Pctf_val}\n\n{b Example OCaml}\n\n [val x: T]  "]
    val pctf_inherit : class_type -> class_type_field[@@ocaml.doc
                                                       " [pctf_inherit] constructs an {! Ast.Pctf_inherit}\n\n{b Example OCaml}\n\n [inherit CT]  "]
    [@@@ocaml.text "{2 Class type}"]
    val pcty_open : open_description -> class_type -> class_type[@@ocaml.doc
                                                                  " [pcty_open] constructs an {! Ast.Pcty_open}\n\n{b Example OCaml}\n\n [let open M in CT]  "]
    val pcty_extension : extension -> class_type[@@ocaml.doc
                                                  " [pcty_extension] constructs an {! Ast.Pcty_extension}\n\n{b Example OCaml}\n\n [%id]  "]
    val pcty_arrow : arg_label -> core_type -> class_type -> class_type
    [@@ocaml.doc
      " [pcty_arrow] constructs an {! Ast.Pcty_arrow}\n\n{b Example OCaml}\n\n [Pcty_arrow(lbl, T, CT)] represents:\n\n          - [T -> CT] when [lbl] is {{!Asttypes.arg_label.Nolabel} [Nolabel]},\n          - [~l:T -> CT] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled l]},\n          - [?l:T -> CT] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]}.  "]
    val pcty_signature : class_signature -> class_type[@@ocaml.doc
                                                        " [pcty_signature] constructs an {! Ast.Pcty_signature}\n\n{b Example OCaml}\n\n [object ... end]  "]
    val pcty_constr : longident loc -> core_type list -> class_type[@@ocaml.doc
                                                                    " [pcty_constr] constructs an {! Ast.Pcty_constr}\n\n{b Example OCaml}\n\n - [c]\n          - [['a1, ..., 'an] c]  "]
    [@@@ocaml.text "{2 Class structure}"]
    val class_structure :
      self:pattern -> fields:class_field list -> class_structure[@@ocaml.doc
                                                                  " [class_structure] constructs an {! Ast.class_structure}\n\n{b Example OCaml}\n\n Values of type {!class_structure} represents:\n\n    - [object(selfpat) ... end]\n    - [object ... end] when {{!class_structure.pcstr_self} [pcstr_self]} is\n      {{!pattern_desc.Ppat_any} [Ppat_any]}  "]
    [@@@ocaml.text "{2 Class signature}"]
    val class_signature :
      self:core_type -> fields:class_type_field list -> class_signature
    [@@ocaml.doc
      " [class_signature] constructs an {! Ast.class_signature}\n\n{b Example OCaml}\n\n Values of type [class_signature] represents:\n\n    - [object('selfpat) ... end]\n    - [object ... end] when {{!class_signature.pcsig_self} [pcsig_self]} is\n      {{!core_type_desc.Ptyp_any} [Ptyp_any]}  "]
    [@@@ocaml.text "{2 Class field}"]
    val pcf_extension : extension -> class_field[@@ocaml.doc
                                                  " [pcf_extension] constructs an {! Ast.Pcf_extension}\n\n{b Example OCaml}\n\n [[%%id]]  "]
    val pcf_attribute : attribute -> class_field[@@ocaml.doc
                                                  " [pcf_attribute] constructs an {! Ast.Pcf_attribute}\n\n{b Example OCaml}\n\n [[@@@id]]  "]
    val pcf_initializer : expression -> class_field[@@ocaml.doc
                                                     " [pcf_initializer] constructs an {! Ast.Pcf_initializer}\n\n{b Example OCaml}\n\n [initializer E]  "]
    val pcf_constraint : (core_type * core_type) -> class_field[@@ocaml.doc
                                                                 " [pcf_constraint] constructs an {! Ast.Pcf_constraint}\n\n{b Example OCaml}\n\n [constraint T1 = T2]  "]
    val pcf_method :
      (label loc * private_flag * class_field_kind) -> class_field[@@ocaml.doc
                                                                    " [pcf_method] constructs an {! Ast.Pcf_method}\n\n{b Example OCaml}\n\n - [method x = E] ([E] can be a\n            {{!expression_desc.Pexp_poly} [Pexp_poly]})\n          - [method virtual x: T] ([T] can be a\n            {{!core_type_desc.Ptyp_poly} [Ptyp_poly]})  "]
    val pcf_val :
      (label loc * mutable_flag * class_field_kind) -> class_field[@@ocaml.doc
                                                                    " [pcf_val] constructs an {! Ast.Pcf_val}\n\n{b Example OCaml}\n\n [Pcf_val(x,flag, kind)] represents:\n\n          - [val x = E] when [flag] is\n            {{!Asttypes.mutable_flag.Immutable} [Immutable]} and [kind] is\n            {{!class_field_kind.Cfk_concrete} [Cfk_concrete(Fresh, E)]}\n          - [val virtual x: T] when [flag] is\n            {{!Asttypes.mutable_flag.Immutable} [Immutable]} and [kind] is\n            {{!class_field_kind.Cfk_virtual} [Cfk_virtual(T)]}\n          - [val mutable x = E] when [flag] is\n            {{!Asttypes.mutable_flag.Mutable} [Mutable]} and [kind] is\n            {{!class_field_kind.Cfk_concrete} [Cfk_concrete(Fresh, E)]}\n          - [val mutable virtual x: T] when [flag] is\n            {{!Asttypes.mutable_flag.Mutable} [Mutable]} and [kind] is\n            {{!class_field_kind.Cfk_virtual} [Cfk_virtual(T)]}  "]
    val pcf_inherit :
      override_flag -> class_expr -> string loc option -> class_field
    [@@ocaml.doc
      " [pcf_inherit] constructs an {! Ast.Pcf_inherit}\n\n{b Example OCaml}\n\n [Pcf_inherit(flag, CE, s)] represents:\n\n          - [inherit CE] when [flag] is\n            {{!Asttypes.override_flag.Fresh} [Fresh]} and [s] is [None],\n          - [inherit CE as x] when [flag] is\n            {{!Asttypes.override_flag.Fresh} [Fresh]} and [s] is [Some x],\n          - [inherit! CE] when [flag] is\n            {{!Asttypes.override_flag.Override} [Override]} and [s] is [None],\n          - [inherit! CE as x] when [flag] is\n            {{!Asttypes.override_flag.Override} [Override]} and [s] is [Some x]\n       "]
    [@@@ocaml.text "{2 Class expr}"]
    val pcl_open : open_description -> class_expr -> class_expr[@@ocaml.doc
                                                                 " [pcl_open] constructs an {! Ast.Pcl_open}\n\n{b Example OCaml}\n\n [let open M in CE]  "]
    val pcl_extension : extension -> class_expr[@@ocaml.doc
                                                 " [pcl_extension] constructs an {! Ast.Pcl_extension}\n\n{b Example OCaml}\n\n [[%id]]  "]
    val pcl_constraint : class_expr -> class_type -> class_expr[@@ocaml.doc
                                                                 " [pcl_constraint] constructs an {! Ast.Pcl_constraint}\n\n{b Example OCaml}\n\n [(CE : CT)]  "]
    val pcl_let : rec_flag -> value_binding list -> class_expr -> class_expr
    [@@ocaml.doc
      " [pcl_let] constructs an {! Ast.Pcl_let}\n\n{b Example OCaml}\n\n [Pcl_let(rec, [(P1, E1); ... ; (Pn, En)], CE)] represents:\n\n          - [let P1 = E1 and ... and Pn = EN in CE] when [rec] is\n            {{!Asttypes.rec_flag.Nonrecursive} [Nonrecursive]},\n          - [let rec P1 = E1 and ... and Pn = EN in CE] when [rec] is\n            {{!Asttypes.rec_flag.Recursive} [Recursive]}.  "]
    val pcl_apply : class_expr -> (arg_label * expression) list -> class_expr
    [@@ocaml.doc
      " [pcl_apply] constructs an {! Ast.Pcl_apply}\n\n{b Example OCaml}\n\n [Pcl_apply(CE, [(l1,E1) ; ... ; (ln,En)])] represents\n          [CE ~l1:E1 ... ~ln:En]. [li] can be empty (non labeled argument) or\n          start with [?] (optional argument).\n\n          Invariant: [n > 0]  "]
    val pcl_fun :
      arg_label -> expression option -> pattern -> class_expr -> class_expr
    [@@ocaml.doc
      " [pcl_fun] constructs an {! Ast.Pcl_fun}\n\n{b Example OCaml}\n\n [Pcl_fun(lbl, exp0, P, CE)] represents:\n\n          - [fun P -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Nolabel} [Nolabel]} and [exp0] is [None],\n          - [fun ~l:P -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Labelled} [Labelled l]} and [exp0] is [None],\n          - [fun ?l:P -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is [None],\n          - [fun ?l:(P = E0) -> CE] when [lbl] is\n            {{!Asttypes.arg_label.Optional} [Optional l]} and [exp0] is\n            [Some E0].  "]
    val pcl_structure : class_structure -> class_expr[@@ocaml.doc
                                                       " [pcl_structure] constructs an {! Ast.Pcl_structure}\n\n{b Example OCaml}\n\n [object ... end]  "]
    val pcl_constr : longident loc -> core_type list -> class_expr[@@ocaml.doc
                                                                    " [pcl_constr] constructs an {! Ast.Pcl_constr}\n\n{b Example OCaml}\n\n [c] and [['a1, ..., 'an] c]  "]
    [@@@ocaml.text "{2 Case}"]
    val case :
      lhs:pattern -> guard:expression option -> rhs:expression -> case
    [@@ocaml.doc
      " [case] constructs an {! Ast.case}\n\n{b Example OCaml}\n\n Values of type {!case} represents [(P -> E)] or [(P when E0 -> E)]  "]
    [@@@ocaml.text "{2 Binding op}"]
    val binding_op :
      op:string loc -> pat:pattern -> exp:expression -> binding_op[@@ocaml.doc
                                                                    " [binding_op] constructs an {! Ast.binding_op}\n\n "]
    [@@@ocaml.text "{2 Attribute}"]
    val attribute : name:string loc -> payload:payload -> attribute[@@ocaml.doc
                                                                    " [attribute] constructs an {! Ast.attribute}\n\n{b Example OCaml}\n\n Attributes such as [[@id ARG]] and [[@@id ARG]].\n\n    Metadata containers passed around within the AST. The compiler ignores\n    unknown attributes.  "]
    [@@@ocaml.text "{2 'a open infos}"]
    val open_infos : expr:'a -> override:override_flag -> 'a open_infos
    [@@ocaml.doc
      " [open_infos] constructs an {! Ast.'a open_infos}\n\n{b Example OCaml}\n\n Values of type ['a open_infos] represents:\n\n    - [open! X] when {{!open_infos.popen_override} [popen_override]} is\n      {{!Asttypes.override_flag.Override} [Override]} (silences the \"used\n      identifier shadowing\" warning)\n    - [open  X] when {{!open_infos.popen_override} [popen_override]} is\n      {{!Asttypes.override_flag.Fresh} [Fresh]}  "]
    [@@@ocaml.text "{2 'a include infos}"]
    val include_infos : 'a -> 'a include_infos[@@ocaml.doc
                                                " [include_infos] constructs an {! Ast.'a include_infos}\n\n "]
    [@@@ocaml.text "{2 'a class infos}"]
    val class_infos :
      virt:virtual_flag ->
        params:(core_type * (variance * injectivity)) list ->
          name:string loc -> expr:'a -> 'a class_infos[@@ocaml.doc
                                                        " [class_infos] constructs an {! Ast.'a class_infos}\n\n{b Example OCaml}\n\n Values of type [class_expr class_infos] represents:\n\n    - [class c = ...]\n    - [class ['a1,...,'an] c = ...]\n    - [class virtual c = ...]\n\n    They are also used for \"class type\" declaration.  "]
  end
module M : Intf =
  struct
    let attribute ~loc  =
      ((fun ~name ->
          ((fun ~payload ->
              { attr_name = name; attr_payload = payload; attr_loc = loc })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let binding_op ~loc  =
      ((fun ~op ->
          ((fun ~pat ->
              ((fun ~exp ->
                  {
                    pbop_op = op;
                    pbop_pat = pat;
                    pbop_exp = exp;
                    pbop_loc = loc
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let case ~lhs  =
      ((fun ~guard ->
          ((fun ~rhs -> { pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let pcl_constr ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_constr (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_structure ~loc  =
      ((fun x0 ->
          { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_structure x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_fun ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  ((fun x3 ->
                      {
                        pcl_attributes = [];
                        pcl_loc = loc;
                        pcl_desc = (Pcl_fun (x0, x1, x2, x3))
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_apply ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_apply (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_let ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pcl_attributes = [];
                    pcl_loc = loc;
                    pcl_desc = (Pcl_let (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_extension ~loc  =
      ((fun x0 ->
          { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_inherit ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pcf_attributes = [];
                    pcf_loc = loc;
                    pcf_desc = (Pcf_inherit (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_val ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_val x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_method ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_method x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_constraint ~loc  =
      ((fun x0 ->
          {
            pcf_attributes = [];
            pcf_loc = loc;
            pcf_desc = (Pcf_constraint x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_initializer ~loc  =
      ((fun x0 ->
          {
            pcf_attributes = [];
            pcf_loc = loc;
            pcf_desc = (Pcf_initializer x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_attribute ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_attribute x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_extension ~loc  =
      ((fun x0 ->
          { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let class_infos ~loc  =
      ((fun ~virt ->
          ((fun ~params ->
              ((fun ~name ->
                  ((fun ~expr ->
                      {
                        pci_virt = virt;
                        pci_params = params;
                        pci_name = name;
                        pci_expr = expr;
                        pci_loc = loc;
                        pci_attributes = []
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let class_signature ~self  =
      ((fun ~fields -> { pcsig_self = self; pcsig_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let class_structure ~self  =
      ((fun ~fields -> { pcstr_self = self; pcstr_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let pcty_constr ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcty_attributes = [];
                pcty_loc = loc;
                pcty_desc = (Pcty_constr (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_signature ~loc  =
      ((fun x0 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_signature x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_arrow ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pcty_attributes = [];
                    pcty_loc = loc;
                    pcty_desc = (Pcty_arrow (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_extension ~loc  =
      ((fun x0 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pcty_attributes = [];
                pcty_loc = loc;
                pcty_desc = (Pcty_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_inherit ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_inherit x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_val ~loc  =
      ((fun x0 ->
          { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_val x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_method ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_method x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_constraint ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_constraint x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_attribute ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_attribute x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_extension ~loc  =
      ((fun x0 ->
          {
            pctf_attributes = [];
            pctf_loc = loc;
            pctf_desc = (Pctf_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let constructor_declaration ~loc  =
      ((fun ~name ->
          ((fun ~vars ->
              ((fun ~args ->
                  ((fun ~res ->
                      {
                        pcd_name = name;
                        pcd_vars = vars;
                        pcd_args = args;
                        pcd_res = res;
                        pcd_loc = loc;
                        pcd_attributes = []
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_any ~loc  =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = Ptyp_any
      }
    let ptyp_var ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_var x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_arrow ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    ptyp_loc_stack = [];
                    ptyp_attributes = [];
                    ptyp_loc = loc;
                    ptyp_desc = (Ptyp_arrow (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_tuple ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_tuple x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_constr ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_constr (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_object ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_object (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_class ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_class (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_alias ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_alias (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_variant ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    ptyp_loc_stack = [];
                    ptyp_attributes = [];
                    ptyp_loc = loc;
                    ptyp_desc = (Ptyp_variant (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_poly ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_poly (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_package ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_package x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_extension ~loc  =
      ((fun x0 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_string ~loc  =
      ((fun x0 -> { pdira_loc = loc; pdira_desc = (Pdir_string x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_int ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pdira_loc = loc; pdira_desc = (Pdir_int (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_ident ~loc  =
      ((fun x0 -> { pdira_loc = loc; pdira_desc = (Pdir_ident x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_bool ~loc  =
      ((fun x0 -> { pdira_loc = loc; pdira_desc = (Pdir_bool x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_ident ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_ident x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_constant ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_constant x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_let ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_let (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_function ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_function x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_fun ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  ((fun x3 ->
                      {
                        pexp_loc_stack = [];
                        pexp_attributes = [];
                        pexp_loc = loc;
                        pexp_desc = (Pexp_fun (x0, x1, x2, x3))
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_apply ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_apply (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_match ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_match (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_try ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_try (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_tuple ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_tuple x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_construct ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_construct (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_variant ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_variant (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_record ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_record (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_field ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_field (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_setfield ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_setfield (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_array ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_array x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_ifthenelse ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_ifthenelse (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_sequence ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_sequence (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_while ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_while (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_for ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  ((fun x3 ->
                      ((fun x4 ->
                          {
                            pexp_loc_stack = [];
                            pexp_attributes = [];
                            pexp_loc = loc;
                            pexp_desc = (Pexp_for (x0, x1, x2, x3, x4))
                          })
                      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                        ][@ppxlib.migration.stop_taking
                                                           ][@ppxlib.migration.stop_taking
                                                              ][@ppxlib.migration.stop_taking
                                                                 ][@ppxlib.migration.stop_taking
                                                                    ]))
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_coerce ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_coerce (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_send ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_send (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_new ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_new x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_setinstvar ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_setinstvar (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_override ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_override x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letmodule ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_letmodule (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letexception ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_letexception (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_assert ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_assert x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_lazy ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_lazy x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_poly ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_poly (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_object ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_object x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_newtype ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_newtype (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_pack ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_pack x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letop ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_letop x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_extension ~loc  =
      ((fun x0 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_unreachable ~loc  =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = Pexp_unreachable
      }
    let extension_constructor ~loc  =
      ((fun ~name ->
          ((fun ~kind ->
              {
                pext_name = name;
                pext_kind = kind;
                pext_loc = loc;
                pext_attributes = []
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let include_infos ~loc  =
      ((fun mod_ ->
          { pincl_mod = mod_; pincl_loc = loc; pincl_attributes = [] })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let label_declaration ~loc  =
      ((fun ~name ->
          ((fun ~mutable_ ->
              ((fun ~type_ ->
                  {
                    pld_name = name;
                    pld_mutable = mutable_;
                    pld_type = type_;
                    pld_loc = loc;
                    pld_attributes = []
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let letop ~let_  =
      ((fun ~ands -> ((fun ~body -> { let_; ands; body })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let location ~start  =
      ((fun ~end_ ->
          ((fun ~ghost ->
              { loc_start = start; loc_end = end_; loc_ghost = ghost })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let module_binding ~loc  =
      ((fun ~name ->
          ((fun ~expr ->
              {
                pmb_name = name;
                pmb_expr = expr;
                pmb_attributes = [];
                pmb_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let module_declaration ~loc  =
      ((fun ~name ->
          ((fun ~type_ ->
              {
                pmd_name = name;
                pmd_type = type_;
                pmd_attributes = [];
                pmd_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_ident ~loc  =
      ((fun x0 ->
          { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_ident x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_structure ~loc  =
      ((fun x0 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_structure x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_functor ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmod_attributes = [];
                pmod_loc = loc;
                pmod_desc = (Pmod_functor (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_apply ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmod_attributes = [];
                pmod_loc = loc;
                pmod_desc = (Pmod_apply (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmod_attributes = [];
                pmod_loc = loc;
                pmod_desc = (Pmod_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_unpack ~loc  =
      ((fun x0 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_unpack x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_extension ~loc  =
      ((fun x0 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let module_substitution ~loc  =
      ((fun ~name ->
          ((fun ~manifest ->
              {
                pms_name = name;
                pms_manifest = manifest;
                pms_attributes = [];
                pms_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_ident ~loc  =
      ((fun x0 ->
          { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_ident x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_signature ~loc  =
      ((fun x0 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_signature x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_functor ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmty_attributes = [];
                pmty_loc = loc;
                pmty_desc = (Pmty_functor (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_with ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pmty_attributes = [];
                pmty_loc = loc;
                pmty_desc = (Pmty_with (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_typeof ~loc  =
      ((fun x0 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_typeof x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_extension ~loc  =
      ((fun x0 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_alias ~loc  =
      ((fun x0 ->
          { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_alias x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let module_type_declaration ~loc  =
      ((fun ~name ->
          ((fun ~type_ ->
              {
                pmtd_name = name;
                pmtd_type = type_;
                pmtd_attributes = [];
                pmtd_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let otag ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                pof_attributes = [];
                pof_loc = loc;
                pof_desc = (Otag (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let oinherit ~loc  =
      ((fun x0 ->
          { pof_attributes = []; pof_loc = loc; pof_desc = (Oinherit x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let open_infos ~loc  =
      ((fun ~expr ->
          ((fun ~override ->
              {
                popen_expr = expr;
                popen_override = override;
                popen_loc = loc;
                popen_attributes = []
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_any ~loc  =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = Ppat_any
      }
    let ppat_var ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_var x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_alias ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_alias (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constant ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_constant x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_interval ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_interval (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_tuple ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_tuple x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_construct ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_construct (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_variant ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_variant (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_record ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_record (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_array ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_array x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_or ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_or (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constraint ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_constraint (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_type ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_type x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_lazy ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_lazy x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_unpack ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_unpack x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_exception ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_exception x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_extension ~loc  =
      ((fun x0 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_extension x0)
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_open ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              {
                ppat_loc_stack = [];
                ppat_attributes = [];
                ppat_loc = loc;
                ppat_desc = (Ppat_open (x0, x1))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let position ~fname  =
      ((fun ~lnum ->
          ((fun ~bol ->
              ((fun ~cnum ->
                  {
                    pos_fname = fname;
                    pos_lnum = lnum;
                    pos_bol = bol;
                    pos_cnum = cnum
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let rtag ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              ((fun x2 ->
                  {
                    prf_attributes = [];
                    prf_loc = loc;
                    prf_desc = (Rtag (x0, x1, x2))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let rinherit ~loc  =
      ((fun x0 ->
          { prf_attributes = []; prf_loc = loc; prf_desc = (Rinherit x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_value ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_value x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_type ~loc  =
      ((fun x0 ->
          ((fun x1 -> { psig_loc = loc; psig_desc = (Psig_type (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_typesubst ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_typesubst x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_typext ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_typext x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_exception ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_exception x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_module ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_module x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_modsubst ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_modsubst x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_recmodule ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_recmodule x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_modtype ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_modtype x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_modtypesubst ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_modtypesubst x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_open ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_open x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_include ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_include x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_class ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_class x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_class_type ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_class_type x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_attribute ~loc  =
      ((fun x0 -> { psig_loc = loc; psig_desc = (Psig_attribute x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_extension ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              { psig_loc = loc; psig_desc = (Psig_extension (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_eval ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_eval (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_value ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_value (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_primitive ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_primitive x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_type ~loc  =
      ((fun x0 ->
          ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_type (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_typext ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_typext x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_exception ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_exception x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_module ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_module x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_recmodule ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_recmodule x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_modtype ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_modtype x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_open ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_open x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_class ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_class x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_class_type ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_class_type x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_include ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_include x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_attribute ~loc  =
      ((fun x0 -> { pstr_loc = loc; pstr_desc = (Pstr_attribute x0) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_extension ~loc  =
      ((fun x0 ->
          ((fun x1 ->
              { pstr_loc = loc; pstr_desc = (Pstr_extension (x0, x1)) })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let toplevel_directive ~loc  =
      ((fun ~name ->
          ((fun ~arg -> { pdir_name = name; pdir_arg = arg; pdir_loc = loc })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let type_declaration ~loc  =
      ((fun ~name ->
          ((fun ~params ->
              ((fun ~cstrs ->
                  ((fun ~kind ->
                      ((fun ~private_ ->
                          ((fun ~manifest ->
                              {
                                ptype_name = name;
                                ptype_params = params;
                                ptype_cstrs = cstrs;
                                ptype_kind = kind;
                                ptype_private = private_;
                                ptype_manifest = manifest;
                                ptype_attributes = [];
                                ptype_loc = loc
                              })
                          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                            ]))
                      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                        ]))
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let type_exception ~loc  =
      ((fun constructor ->
          {
            ptyexn_constructor = constructor;
            ptyexn_loc = loc;
            ptyexn_attributes = []
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let type_extension ~loc  =
      ((fun ~path ->
          ((fun ~params ->
              ((fun ~constructors ->
                  ((fun ~private_ ->
                      {
                        ptyext_path = path;
                        ptyext_params = params;
                        ptyext_constructors = constructors;
                        ptyext_private = private_;
                        ptyext_loc = loc;
                        ptyext_attributes = []
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let value_binding ~loc  =
      ((fun ~pat ->
          ((fun ~expr ->
              {
                pvb_pat = pat;
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let value_description ~loc  =
      ((fun ~name ->
          ((fun ~type_ ->
              ((fun ~prim ->
                  {
                    pval_name = name;
                    pval_type = type_;
                    pval_prim = prim;
                    pval_attributes = [];
                    pval_loc = loc
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
  end 
module Make(Loc:sig val loc : Location.t end) : Intf_located =
  struct
    let loc = Loc.loc
    let attribute ~name  =
      ((fun ~payload ->
          { attr_name = name; attr_payload = payload; attr_loc = loc })
      [@ppxlib.migration.stop_taking ])
    let binding_op ~op  =
      ((fun ~pat ->
          ((fun ~exp ->
              { pbop_op = op; pbop_pat = pat; pbop_exp = exp; pbop_loc = loc
              })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let case ~lhs  =
      ((fun ~guard ->
          ((fun ~rhs -> { pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let pcl_constr x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_constr (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_structure x0 =
      { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_structure x0) }
    let pcl_fun x0 =
      ((fun x1 ->
          ((fun x2 ->
              ((fun x3 ->
                  {
                    pcl_attributes = [];
                    pcl_loc = loc;
                    pcl_desc = (Pcl_fun (x0, x1, x2, x3))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_apply x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_apply (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_let x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pcl_attributes = [];
                pcl_loc = loc;
                pcl_desc = (Pcl_let (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_constraint x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcl_extension x0 =
      { pcl_attributes = []; pcl_loc = loc; pcl_desc = (Pcl_extension x0) }
    let pcl_open x0 =
      ((fun x1 ->
          {
            pcl_attributes = [];
            pcl_loc = loc;
            pcl_desc = (Pcl_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_inherit x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pcf_attributes = [];
                pcf_loc = loc;
                pcf_desc = (Pcf_inherit (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcf_val x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_val x0) }
    let pcf_method x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_method x0) }
    let pcf_constraint x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_constraint x0) }
    let pcf_initializer x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_initializer x0) }
    let pcf_attribute x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_attribute x0) }
    let pcf_extension x0 =
      { pcf_attributes = []; pcf_loc = loc; pcf_desc = (Pcf_extension x0) }
    let class_infos ~virt  =
      ((fun ~params ->
          ((fun ~name ->
              ((fun ~expr ->
                  {
                    pci_virt = virt;
                    pci_params = params;
                    pci_name = name;
                    pci_expr = expr;
                    pci_loc = loc;
                    pci_attributes = []
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let class_signature ~self  =
      ((fun ~fields -> { pcsig_self = self; pcsig_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let class_structure ~self  =
      ((fun ~fields -> { pcstr_self = self; pcstr_fields = fields })
      [@ppxlib.migration.stop_taking ])
    let pcty_constr x0 =
      ((fun x1 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_constr (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_signature x0 =
      { pcty_attributes = []; pcty_loc = loc; pcty_desc = (Pcty_signature x0)
      }
    let pcty_arrow x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pcty_attributes = [];
                pcty_loc = loc;
                pcty_desc = (Pcty_arrow (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pcty_extension x0 =
      { pcty_attributes = []; pcty_loc = loc; pcty_desc = (Pcty_extension x0)
      }
    let pcty_open x0 =
      ((fun x1 ->
          {
            pcty_attributes = [];
            pcty_loc = loc;
            pcty_desc = (Pcty_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pctf_inherit x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_inherit x0) }
    let pctf_val x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_val x0) }
    let pctf_method x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_method x0) }
    let pctf_constraint x0 =
      {
        pctf_attributes = [];
        pctf_loc = loc;
        pctf_desc = (Pctf_constraint x0)
      }
    let pctf_attribute x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_attribute x0)
      }
    let pctf_extension x0 =
      { pctf_attributes = []; pctf_loc = loc; pctf_desc = (Pctf_extension x0)
      }
    let constructor_declaration ~name  =
      ((fun ~vars ->
          ((fun ~args ->
              ((fun ~res ->
                  {
                    pcd_name = name;
                    pcd_vars = vars;
                    pcd_args = args;
                    pcd_res = res;
                    pcd_loc = loc;
                    pcd_attributes = []
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let ptyp_any =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = Ptyp_any
      }
    let ptyp_var x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_var x0)
      }
    let ptyp_arrow x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_arrow (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_tuple x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_tuple x0)
      }
    let ptyp_constr x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_constr (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_object x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_object (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_class x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_class (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_alias x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_alias (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_variant x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                ptyp_loc_stack = [];
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_desc = (Ptyp_variant (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_poly x0 =
      ((fun x1 ->
          {
            ptyp_loc_stack = [];
            ptyp_attributes = [];
            ptyp_loc = loc;
            ptyp_desc = (Ptyp_poly (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ptyp_package x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_package x0)
      }
    let ptyp_extension x0 =
      {
        ptyp_loc_stack = [];
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_desc = (Ptyp_extension x0)
      }
    let pdir_string x0 = { pdira_loc = loc; pdira_desc = (Pdir_string x0) }
    let pdir_int x0 =
      ((fun x1 -> { pdira_loc = loc; pdira_desc = (Pdir_int (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pdir_ident x0 = { pdira_loc = loc; pdira_desc = (Pdir_ident x0) }
    let pdir_bool x0 = { pdira_loc = loc; pdira_desc = (Pdir_bool x0) }
    let pexp_ident x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_ident x0)
      }
    let pexp_constant x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_constant x0)
      }
    let pexp_let x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_let (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_function x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_function x0)
      }
    let pexp_fun x0 =
      ((fun x1 ->
          ((fun x2 ->
              ((fun x3 ->
                  {
                    pexp_loc_stack = [];
                    pexp_attributes = [];
                    pexp_loc = loc;
                    pexp_desc = (Pexp_fun (x0, x1, x2, x3))
                  })
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_apply x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_apply (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_match x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_match (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_try x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_try (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_tuple x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_tuple x0)
      }
    let pexp_construct x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_construct (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_variant x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_variant (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_record x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_record (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_field x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_field (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_setfield x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_setfield (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_array x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_array x0)
      }
    let pexp_ifthenelse x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_ifthenelse (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_sequence x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_sequence (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_while x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_while (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_for x0 =
      ((fun x1 ->
          ((fun x2 ->
              ((fun x3 ->
                  ((fun x4 ->
                      {
                        pexp_loc_stack = [];
                        pexp_attributes = [];
                        pexp_loc = loc;
                        pexp_desc = (Pexp_for (x0, x1, x2, x3, x4))
                      })
                  [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                    ][@ppxlib.migration.stop_taking
                                                       ][@ppxlib.migration.stop_taking
                                                          ][@ppxlib.migration.stop_taking
                                                             ]))
              [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking
                                                ][@ppxlib.migration.stop_taking
                                                   ][@ppxlib.migration.stop_taking
                                                      ]))
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_constraint x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_coerce x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_coerce (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_send x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_send (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_new x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_new x0)
      }
    let pexp_setinstvar x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_setinstvar (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_override x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_override x0)
      }
    let pexp_letmodule x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                pexp_loc_stack = [];
                pexp_attributes = [];
                pexp_loc = loc;
                pexp_desc = (Pexp_letmodule (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letexception x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_letexception (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_assert x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_assert x0)
      }
    let pexp_lazy x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_lazy x0)
      }
    let pexp_poly x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_poly (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_object x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_object x0)
      }
    let pexp_newtype x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_newtype (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_pack x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_pack x0)
      }
    let pexp_open x0 =
      ((fun x1 ->
          {
            pexp_loc_stack = [];
            pexp_attributes = [];
            pexp_loc = loc;
            pexp_desc = (Pexp_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pexp_letop x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_letop x0)
      }
    let pexp_extension x0 =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = (Pexp_extension x0)
      }
    let pexp_unreachable =
      {
        pexp_loc_stack = [];
        pexp_attributes = [];
        pexp_loc = loc;
        pexp_desc = Pexp_unreachable
      }
    let extension_constructor ~name  =
      ((fun ~kind ->
          {
            pext_name = name;
            pext_kind = kind;
            pext_loc = loc;
            pext_attributes = []
          })
      [@ppxlib.migration.stop_taking ])
    let include_infos mod_ =
      { pincl_mod = mod_; pincl_loc = loc; pincl_attributes = [] }
    let label_declaration ~name  =
      ((fun ~mutable_ ->
          ((fun ~type_ ->
              {
                pld_name = name;
                pld_mutable = mutable_;
                pld_type = type_;
                pld_loc = loc;
                pld_attributes = []
              })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let letop ~let_  =
      ((fun ~ands -> ((fun ~body -> { let_; ands; body })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let location ~start  =
      ((fun ~end_ ->
          ((fun ~ghost ->
              { loc_start = start; loc_end = end_; loc_ghost = ghost })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let module_binding ~name  =
      ((fun ~expr ->
          {
            pmb_name = name;
            pmb_expr = expr;
            pmb_attributes = [];
            pmb_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let module_declaration ~name  =
      ((fun ~type_ ->
          {
            pmd_name = name;
            pmd_type = type_;
            pmd_attributes = [];
            pmd_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let pmod_ident x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_ident x0) }
    let pmod_structure x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_structure x0)
      }
    let pmod_functor x0 =
      ((fun x1 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_functor (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_apply x0 =
      ((fun x1 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_apply (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_constraint x0 =
      ((fun x1 ->
          {
            pmod_attributes = [];
            pmod_loc = loc;
            pmod_desc = (Pmod_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmod_unpack x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_unpack x0) }
    let pmod_extension x0 =
      { pmod_attributes = []; pmod_loc = loc; pmod_desc = (Pmod_extension x0)
      }
    let module_substitution ~name  =
      ((fun ~manifest ->
          {
            pms_name = name;
            pms_manifest = manifest;
            pms_attributes = [];
            pms_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let pmty_ident x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_ident x0) }
    let pmty_signature x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_signature x0)
      }
    let pmty_functor x0 =
      ((fun x1 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_functor (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_with x0 =
      ((fun x1 ->
          {
            pmty_attributes = [];
            pmty_loc = loc;
            pmty_desc = (Pmty_with (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pmty_typeof x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_typeof x0) }
    let pmty_extension x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_extension x0)
      }
    let pmty_alias x0 =
      { pmty_attributes = []; pmty_loc = loc; pmty_desc = (Pmty_alias x0) }
    let module_type_declaration ~name  =
      ((fun ~type_ ->
          {
            pmtd_name = name;
            pmtd_type = type_;
            pmtd_attributes = [];
            pmtd_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let otag x0 =
      ((fun x1 ->
          { pof_attributes = []; pof_loc = loc; pof_desc = (Otag (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let oinherit x0 =
      { pof_attributes = []; pof_loc = loc; pof_desc = (Oinherit x0) }
    let open_infos ~expr  =
      ((fun ~override ->
          {
            popen_expr = expr;
            popen_override = override;
            popen_loc = loc;
            popen_attributes = []
          })
      [@ppxlib.migration.stop_taking ])
    let ppat_any =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = Ppat_any
      }
    let ppat_var x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_var x0)
      }
    let ppat_alias x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_alias (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constant x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_constant x0)
      }
    let ppat_interval x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_interval (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_tuple x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_tuple x0)
      }
    let ppat_construct x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_construct (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_variant x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_variant (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_record x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_record (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_array x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_array x0)
      }
    let ppat_or x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_or (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_constraint x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_constraint (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let ppat_type x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_type x0)
      }
    let ppat_lazy x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_lazy x0)
      }
    let ppat_unpack x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_unpack x0)
      }
    let ppat_exception x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_exception x0)
      }
    let ppat_extension x0 =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = (Ppat_extension x0)
      }
    let ppat_open x0 =
      ((fun x1 ->
          {
            ppat_loc_stack = [];
            ppat_attributes = [];
            ppat_loc = loc;
            ppat_desc = (Ppat_open (x0, x1))
          })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let position ~fname  =
      ((fun ~lnum ->
          ((fun ~bol ->
              ((fun ~cnum ->
                  {
                    pos_fname = fname;
                    pos_lnum = lnum;
                    pos_bol = bol;
                    pos_cnum = cnum
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let rtag x0 =
      ((fun x1 ->
          ((fun x2 ->
              {
                prf_attributes = [];
                prf_loc = loc;
                prf_desc = (Rtag (x0, x1, x2))
              })
          [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ]
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let rinherit x0 =
      { prf_attributes = []; prf_loc = loc; prf_desc = (Rinherit x0) }
    let psig_value x0 = { psig_loc = loc; psig_desc = (Psig_value x0) }
    let psig_type x0 =
      ((fun x1 -> { psig_loc = loc; psig_desc = (Psig_type (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let psig_typesubst x0 =
      { psig_loc = loc; psig_desc = (Psig_typesubst x0) }
    let psig_typext x0 = { psig_loc = loc; psig_desc = (Psig_typext x0) }
    let psig_exception x0 =
      { psig_loc = loc; psig_desc = (Psig_exception x0) }
    let psig_module x0 = { psig_loc = loc; psig_desc = (Psig_module x0) }
    let psig_modsubst x0 = { psig_loc = loc; psig_desc = (Psig_modsubst x0) }
    let psig_recmodule x0 =
      { psig_loc = loc; psig_desc = (Psig_recmodule x0) }
    let psig_modtype x0 = { psig_loc = loc; psig_desc = (Psig_modtype x0) }
    let psig_modtypesubst x0 =
      { psig_loc = loc; psig_desc = (Psig_modtypesubst x0) }
    let psig_open x0 = { psig_loc = loc; psig_desc = (Psig_open x0) }
    let psig_include x0 = { psig_loc = loc; psig_desc = (Psig_include x0) }
    let psig_class x0 = { psig_loc = loc; psig_desc = (Psig_class x0) }
    let psig_class_type x0 =
      { psig_loc = loc; psig_desc = (Psig_class_type x0) }
    let psig_attribute x0 =
      { psig_loc = loc; psig_desc = (Psig_attribute x0) }
    let psig_extension x0 =
      ((fun x1 -> { psig_loc = loc; psig_desc = (Psig_extension (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_eval x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_eval (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_value x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_value (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_primitive x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_primitive x0) }
    let pstr_type x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_type (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let pstr_typext x0 = { pstr_loc = loc; pstr_desc = (Pstr_typext x0) }
    let pstr_exception x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_exception x0) }
    let pstr_module x0 = { pstr_loc = loc; pstr_desc = (Pstr_module x0) }
    let pstr_recmodule x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_recmodule x0) }
    let pstr_modtype x0 = { pstr_loc = loc; pstr_desc = (Pstr_modtype x0) }
    let pstr_open x0 = { pstr_loc = loc; pstr_desc = (Pstr_open x0) }
    let pstr_class x0 = { pstr_loc = loc; pstr_desc = (Pstr_class x0) }
    let pstr_class_type x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_class_type x0) }
    let pstr_include x0 = { pstr_loc = loc; pstr_desc = (Pstr_include x0) }
    let pstr_attribute x0 =
      { pstr_loc = loc; pstr_desc = (Pstr_attribute x0) }
    let pstr_extension x0 =
      ((fun x1 -> { pstr_loc = loc; pstr_desc = (Pstr_extension (x0, x1)) })
      [@ppxlib.migration.stop_taking ][@ppxlib.migration.stop_taking ])
    let toplevel_directive ~name  =
      ((fun ~arg -> { pdir_name = name; pdir_arg = arg; pdir_loc = loc })
      [@ppxlib.migration.stop_taking ])
    let type_declaration ~name  =
      ((fun ~params ->
          ((fun ~cstrs ->
              ((fun ~kind ->
                  ((fun ~private_ ->
                      ((fun ~manifest ->
                          {
                            ptype_name = name;
                            ptype_params = params;
                            ptype_cstrs = cstrs;
                            ptype_kind = kind;
                            ptype_private = private_;
                            ptype_manifest = manifest;
                            ptype_attributes = [];
                            ptype_loc = loc
                          })
                      [@ppxlib.migration.stop_taking ]))
                  [@ppxlib.migration.stop_taking ]))
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let type_exception constructor =
      {
        ptyexn_constructor = constructor;
        ptyexn_loc = loc;
        ptyexn_attributes = []
      }
    let type_extension ~path  =
      ((fun ~params ->
          ((fun ~constructors ->
              ((fun ~private_ ->
                  {
                    ptyext_path = path;
                    ptyext_params = params;
                    ptyext_constructors = constructors;
                    ptyext_private = private_;
                    ptyext_loc = loc;
                    ptyext_attributes = []
                  })
              [@ppxlib.migration.stop_taking ]))
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
    let value_binding ~pat  =
      ((fun ~expr ->
          {
            pvb_pat = pat;
            pvb_expr = expr;
            pvb_attributes = [];
            pvb_loc = loc
          })
      [@ppxlib.migration.stop_taking ])
    let value_description ~name  =
      ((fun ~type_ ->
          ((fun ~prim ->
              {
                pval_name = name;
                pval_type = type_;
                pval_prim = prim;
                pval_attributes = [];
                pval_loc = loc
              })
          [@ppxlib.migration.stop_taking ]))
      [@ppxlib.migration.stop_taking ])
  end 
