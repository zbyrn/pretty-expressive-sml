signature COST_MODEL = sig
  (** The cost factory interface.

      A valid cost factory should also satisfy the following contracts.

      - [le] is a total ordering.
      - If [le a b] and [le c d] then [le (combine a c) (combine b d)]
      - If [a] <= [b], then [le (text a l) (text b l)]
      - If [a] <= [b], then [le (newline a) (newline b)]
      - [text c (a + b) = combine (text c a) (text (c + a) b)]
      - [combine] is associative and has the identity equal to [text 0 0]
      - [text c 0] = [text 0 0] for any [c]
      - If [a] <= [b], then
          [le (two_columns_overflow a) (two_columns_overflow b)]
      - If [a] <= [b], then [le (two_columns_bias a) (two_columns_bias b)]

      See DefaultCostFn for examples of cost factories. *)

  (* A type for cost *)
  type t

  (* [text (pw, c, l)] calculates a cost for a text placement at column position
   * [c] with length [l] with page width [pw] *)
  val text : int * int * int -> t

  (* [newline (pw, i)] calculates a cost for a newline and indentation at level
   * [i] with page width [pw] *)
  val newline : int * int -> t

  (* [combine (x, y)] combines the costs [x] and [y] together *)
  val combine : t * t -> t

  (* [le (x, y)] tests if the cost [x] is less than or equal to the cost [y]. *)
  val le : t * t -> bool

  (* [limit pagewidth] is the computation width limit. *)
  val limit: int -> int

  (* [toString c] produces a string representation of a cost [c] *)
  val toString : t -> string

  (* [debugFormat pw (s, isTainted, cost)] produces a debugging string
     from the output of the core printer. *)
  val debugFormat : int -> string * bool * string -> string
end

signature PRINTER = sig
  (* The [doc] type *)
  type doc

  (* The [cost] type *)
  type cost

  (* The type of a renderer *)
  type renderer = string -> unit

  (* [print renderer d] prints the document [d] by repeatedly calling
   * [renderer]. *)
  val pprint : renderer -> int -> doc -> unit

  (* [format d] renders a document [d] *)
  val format : int -> doc -> string

  (* [formatDebug] is similar to [pretty_format] but the debugging
   * information is included as a part of the output string. *)
  val formatDebug : int -> doc -> string

  (** Document Constructions **)

  (* [text s] is a document for textual content [s];
   * [s] must not contain a newline. *)
  val text : string -> doc

  (* [blank n] is a document with n spaces *)
  val blank : int -> doc

  (* [newline s] is a document for a newline.
   * When [s] is [NONE], it flattens to [fail].
   * When [s] is [SOME s'], it flattens to [text s'].
   * See [flatten] for more details.
   * After the newline is inserted, indentation will be added, according to the
   * current indentation level. *)
  val newline : (string option) -> doc

  (* [nl] is a document for a newline that flattens to a single space. *)
  val nl : doc

  (* [break] is a document for a newline that flattens to an empty string. *)
  val break : doc

  (* [hardnl] is a document for a newline that fails to flatten. *)
  val hardnl : doc

  (** Combinators **)

  (* [a ^^ b] is a document for concatenation of documents [a] and [b] without
   * alignment.
   * In the paper, the symbol [<>] is used for the operator.
   * We use [^^] in the SML implementation instead to avoid shadowing the
   * built-in not equal operator.
   * This operator also known as the unaligned concatenation, which is widely
   * used in traditional pretty printers. *)
  val ^^ : doc * doc -> doc

  (* [a <|> b] is a document for a choice between document [a] and [b]. *)
  val <|> : doc * doc -> doc

  (** Indentation documents **)

  (* [align d] is a document that aligns [d] at the column position.
   * The aligned concatenation operator <+> is a derived combinator that
   * composes ^^ and align together. It is especially useful for languages that
   * uses the box model for code styling. *)
  val align : doc -> doc

  (* [nest n d] is a document that increments the indentation level by [n] when
   * rendering [d] *)
  val nest : int -> doc -> doc

  (* [reset d] is a document that resets indentation level to 0 in [d].
   * This is especially useful for formatting multi-line strings and multi-line
   * comments *)
  val reset : doc -> doc

  (** Cost document **)

  (* [cost c d] is a document that artificially adds cost [c] to [d]. *)
  val cost : cost -> doc -> doc

  (** Failure document **)

  (* A document that always fails. It interacts with [<|>]; failing branches are
   * pruned away. *)
  val fail : doc

  (** Derived combinators **)

  (* [flatten d] is a document that replaces newlines and indentation spaces
   * with what's specificied in [newline] when rendering [d]. *)
  val flatten : doc -> doc

  (* [group d] is a shorthand for [d <|> flatten d].
   * This combinator is a part of most traditional pretty printers. *)
  val group : doc -> doc

  (* [a <+> b] is a shorthand for [a ^^ align b]
   * It is also known as the aligned concatenation. *)
  val <+> : doc * doc -> doc

  (* [a <$> b] is a shorthand for [a ^^ hardnl ^^ b] *)
  val <$> : doc * doc -> doc

  (* [a <-> b] is a shorthand for [flatten a <+> b].
   * This is especially useful when combined with [hardnl] and [<|>]: it can be
   * used when we want to do aligned concatenation, but don't want the left part
   * to have multiple lines *)
  val <-> : doc * doc -> doc

  (* [foldDoc (op++) ds] is a shorthand for [d1 ++ d2 ++ ... ++ dn] where [d1
   * d2 ... dn] are drawn from [ds] *)
  val foldDoc : (doc * doc -> doc) -> doc list -> doc

  (* [vcat ds] is a shorthand for [d1 <$> d2 <$> ... <$> dn] *)
  val vcat : doc list -> doc

  (* [hcat ds] is a shorthand for [d1 <-> d2 <-> ... <-> dn] *)
  val hcat : doc list -> doc

  (* Equivalent to [text ""] *)
  val empty : doc

  (* Equivalent to [text " "] *)
  val space : doc

  (* Equivalent to [text ","] *)
  val comma : doc

  (* Equivalent to [text "["] *)
  val lbrack : doc

  (* Equivalent to [text "]"] *)
  val rbrack: doc

  (* Equivalent to [text "{"] *)
  val lbrace : doc

  (* Equivalent to [text "}"] *)
  val rbrace : doc

  (* Equivalent to [text "("] *)
  val lparen : doc

  (* Equivalent to [text ")"] *)
  val rparen : doc

  (* Equivalent to [text "\""] *)
  val dquote : doc
end
