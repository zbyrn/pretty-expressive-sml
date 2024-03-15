functor PrinterFn(C : COST_MODEL) : PRINTER = struct
  (* magic number *)
  val ParamMemoLimit = 7

  type renderer = string -> unit

  datatype 's treeof = One  of 's
                     | Cons of 's treeof * 's treeof

  fun renderTree renderer (t: string treeof) =
    let fun loop (One v) = renderer v
          | loop (Cons (x, y)) = (loop x; loop y)
    in  loop t
    end

  fun unreachable msg = raise Fail ("unreachable: " ^ msg)

  infix 4 ^^ <$> <-> <+>
  infix 1 <|>

  infix 4 <==
  infix 5 ++

  structure Core = struct
    val globalID = ref 0
    fun nextID () = !globalID before (globalID := !globalID + 1)

    structure HashTbl = IntHashTable
    exception MemoTbl
    fun findWith (tbl : 'a HashTbl.hash_table, key, default : unit -> 'a) =
      case HashTbl.find tbl key
        of SOME v => v
         | NONE   =>
             let val v = default ()
             in  HashTbl.insert tbl (key, v); v
             end

    type measure = { last: int, cost: C.t, layout: unit -> string treeof }

    fun (m1 : measure) <== (m2 : measure) =
      #last m1 <= #last m2 andalso C.le (#cost m1, #cost m2)

    datatype measure_set
      = MeasureSet of measure list (* sorted by last in decreasing order *)
      | Tainted of (unit -> measure)

    datatype doc_case
      = Text of string treeof * int
      | Newline of string option
      | Concat of doc * doc
      | Choice of doc * doc
      | Nest of int * doc
      | Align of doc
      | Reset of doc
      | Cost of C.t * doc
      | Blank of int
      | Fail
    withtype doc =
      { dc: doc_case, id: int, memoW: int, nlCnt: int,
        table: measure_set HashTbl.hash_table option }

    type cost = C.t

    val initMemoW = ParamMemoLimit - 1
    fun calcWeight (d : doc) =
      if #memoW d = 0 then initMemoW else #memoW d - 1
    fun initTable w =
      if w = 0 then SOME (HashTbl.mkTable (5, MemoTbl)) else NONE

    val fail : doc =
      { dc = Fail,
        id = nextID (),
        memoW = initMemoW,
        nlCnt = 0,
        table = NONE }

    fun newline (v : string option) : doc =
      { dc = Newline v,
        id = nextID (),
        memoW = initMemoW,
        nlCnt = 1,
        table = NONE }

    fun makeText (s : string treeof) (l : int) : doc =
      { dc = Text (s, l),
        id = nextID (),
        memoW = initMemoW,
        nlCnt = 0,
        table = NONE }

    fun text (s : string) : doc = makeText (One s) (String.size s)

    fun blank (i : int) : doc =
      { dc = Blank i,
        id = nextID (),
        memoW = initMemoW,
        nlCnt = 0,
        table = NONE }

    fun cost (c : C.t) (d : doc) : doc =
      case #dc d
        of Fail => fail
         | Cost (c2, d) => cost (C.combine (c, c2)) d
         | _ =>
             let val memoW = calcWeight d
             in  { dc = Cost (c, d),
                   id = nextID (),
                   memoW = memoW,
                   nlCnt = #nlCnt d,
                   table = initTable memoW }
             end

    fun op^^ (d1 : doc, d2 : doc) : doc =
      case (#dc d1, #dc d2)
        of ((Fail, _) | (_, Fail)) => fail
         | (Text (_, 0), _) => d2
         | (_, Text (_, 0)) => d1
         | (Text (s1, l1), Text (s2, l2)) => makeText (Cons (s1, s2)) (l1 + l2)
         | (_, Cost (c, d2)) => cost c (d1 ^^ d2)
         | (Cost (c, d1), _) => cost c (d1 ^^ d2)
         | _ =>
             let val memoW = Int.min (calcWeight d1, calcWeight d2)
             in  { dc = Concat (d1, d2),
                   id = nextID (),
                   memoW = memoW,
                   nlCnt = #nlCnt d1 + #nlCnt d2,
                   table = initTable memoW }
             end

    fun nest (n : int) (d : doc) : doc =
      case #dc d
        of (Fail | Align _ | Reset _ | Text _) => d
         | Cost (c, d) => cost c (nest n d)
         | _ =>
             let val memoW = calcWeight d
             in  { dc = Nest (n, d),
                   id = nextID (),
                   memoW = memoW,
                   nlCnt = #nlCnt d,
                   table = initTable memoW }
             end

    fun reset (d : doc) : doc =
      case #dc d
        of (Fail | Align _ | Reset _ | Text _) => d
         | Cost (c, d) => cost c (reset d)
         | _ =>
             let val memoW = calcWeight d
             in  { dc = Reset d,
                   id = nextID (),
                   memoW = memoW,
                   nlCnt = #nlCnt d,
                   table = initTable memoW }
             end

    fun align (d : doc) : doc =
      case #dc d
        of (Fail | Align _ | Reset _ | Text _) => d
         | Cost (c, d) => cost c (align d)
         | _ =>
             let val memoW = calcWeight d
             in  { dc = Align d,
                   id = nextID (),
                   memoW = memoW,
                   nlCnt = #nlCnt d,
                   table = initTable memoW }
             end

    fun op<|> (d1 : doc, d2 : doc) : doc =
      case (#dc d1, #dc d2)
        of (Fail, _) => d2
         | (_, Fail) => d1
         | _ =>
             let val memoW = Int.min (calcWeight d1, calcWeight d2)
             in  { dc = Choice (d1, d2),
                   id = nextID (),
                   memoW = memoW,
                   nlCnt = Int.max (#nlCnt d1, #nlCnt d2),
                   table = initTable memoW }
             end

    val empty : doc = text ""

    val hardnl : doc = newline NONE

    fun merge (ml1, Tainted _) = ml1
      | merge (Tainted _, ml2) = ml2
      | merge (MeasureSet ms1, MeasureSet ms2) =
          let fun loop ([], _) = ms2
                | loop (_, []) = ms1
                | loop (m1 :: ms1p, m2 :: ms2p) =
                    if m1 <== m2 then
                      loop (ms1, ms2p)
                    else if m2 <== m1 then
                      loop (ms1p, ms2)
                    else if #last m1 > #last m2 then
                      m1 :: loop (ms1p, ms2)
                    else (* #last m2 > #last m1 *)
                      m2 :: loop (ms1, ms2p)
          in  MeasureSet (loop (ms1, ms2))
          end

    fun op++ (m1 : measure, m2 : measure) : measure =
      { last = #last m2,
        cost = C.combine (#cost m1, #cost m2),
        layout = fn () => (Cons (#layout m1 (), #layout m2 ())) }

    fun invalidateCache ({ dc, table, ... } : doc) =
          (case table of NONE => () | SOME tbl => HashTbl.clear tbl;
           invalidateCacheCase dc)
    and invalidateCacheCase dc =
          (case dc
             of (Concat (d1, d2) | Choice (d1, d2)) =>
                  (invalidateCache d1; invalidateCache d2)
              | (Nest (_, d) | Align d | Reset d | Cost (_, d)) =>
                  invalidateCache d
              | _ => ())

    fun processConcat
        (processLeft : measure -> measure_set, ml1 : measure_set)
        : measure_set =
      case ml1
        of Tainted mt1 =>
             Tainted (fn () =>
               let val m1 = mt1 ()
               in  case processLeft m1
                     of Tainted mt2 => m1 ++ mt2 ()
                      | MeasureSet (m2 :: _) => m1 ++ m2
                      | _ => unreachable "empty measure set"
               end)
         | MeasureSet ms1 =>
             let
               fun doOne m1 =
                 let fun loop (ms2, result, currentBest) =
                       case ms2
                         of [] => List.rev (currentBest :: result)
                          | m2 :: ms2 =>
                              let val current = m1 ++ m2
                              in  if C.le (#cost current, #cost currentBest)
                                  then loop (ms2, result, current)
                                  else loop (ms2, currentBest::result, current)
                              end
                 in  case processLeft m1
                       of Tainted m2 => Tainted (fn () => m1 ++ m2 ())
                        | MeasureSet (m2 :: ms2) =>
                            MeasureSet (loop (ms2, [], m1 ++ m2))
                        | MeasureSet _ => unreachable "empty measure set"
                 end
               fun foldr [m] = doOne m
                 | foldr (m :: ms) = merge (doOne m, foldr ms)
                 | foldr [] = unreachable "empty measure set"
             in
               foldr ms1
             end

    fun memoize limit f : doc * int * int -> measure_set =
      let val allSlots = limit + 1
          fun g (d : doc as { memoW, table, ... }, c, i) =
            if c <= limit andalso i <= limit andalso memoW = 0 then
              let val key = i * allSlots + c
              in  case table
                    of NONE => unreachable "No table in memoize"
                     | SOME tbl => findWith (tbl, key, fn () => f (g, d, c, i))
              end
            else
              f (g, d, c, i)
      in  g
      end

    fun chooseOne (Tainted mt) = mt ()
      | chooseOne (MeasureSet (m :: _)) = m
      | chooseOne _ = unreachable "empty measure set"

    fun makeString (n, c) = String.implode (List.tabulate (n, fn _ => c))

    fun solve renderer pagewidth (d : doc) : { isTainted: bool, cost: cost } =
      let
        val limit = C.limit pagewidth
        fun resolve (self, {dc, ...} : doc, c, i) : measure_set =
          let
            fun core () =
              case dc
                of Text (s, lenS) =>
                     MeasureSet [{ last = c + lenS,
                                   cost = C.text (pagewidth, c, lenS),
                                   layout = fn () => s }]
                 | Newline _ =>
                     MeasureSet [{ last = i,
                                   cost = C.newline (pagewidth, i),
                                   layout = fn () =>
                                     One ("\n" ^ makeString (i, #" ")) }]
                 | Concat (d1, d2) =>
                     processConcat (fn m1 => self (d2, #last m1, i),
                                    self (d1, c, i))
                 | Choice (d1, d2) =>
                     if #nlCnt d1 < #nlCnt d2 then
                       merge (self (d2, c, i), self (d1, c, i))
                     else
                       merge (self (d1, c, i), self (d2, c, i))
                 | Nest (n, d) => self (d, c, i + n)
                 | Align d => self (d, c, c)
                 | Reset d => self (d, c, 0)
                 | Cost (co, d) =>
                     let fun addCost (m : measure) =
                           { last = #last m, layout = #layout m,
                             cost = C.combine (co, #cost m) }
                     in  case self (d, c, i)
                           of MeasureSet ms => MeasureSet (List.map addCost ms)
                            | Tainted ms => Tainted (fn () => addCost (ms ()))
                     end
                 | Blank i =>
                     MeasureSet [{ last = c + i,
                                   cost = C.text (pagewidth, 0, 0),
                                   layout =
                                     fn () => One (makeString (i, #" ")) }]
                 | Fail => unreachable "fails to render"
            val exceeds =
              (case dc of Text (_, len) => c + len > limit orelse i > limit
                        | _ => c > limit orelse i > limit)
          in
            if exceeds then
              Tainted (fn () => chooseOne (core ()))
            else
              core ()
          end
        val (m, isTainted) =
          (case memoize limit resolve (d, 0, 0)
             of MeasureSet (m :: _) => (m, false)
              | Tainted m => (m (), true)
              | _ => unreachable "empty set")
        val layout = #layout m ()
      in
        renderTree renderer layout;
        invalidateCache d;
        { isTainted = isTainted, cost = #cost m }
      end
  end

  open Core

  val comma = text ","
  val lbrack = text "["
  val rbrack = text "]"
  val lbrace = text "{"
  val rbrace = text "}"
  val lparen = text "("
  val rparen = text ")"
  val dquote = text "\""
  val space = text " "
  val nl = newline (SOME " ")
  val break = newline (SOME "")

  fun op<$>(d1, d2) = d1 ^^ hardnl ^^ d2
  local
    val cache = HashTbl.mkTable (1000, MemoTbl)
  in
    fun flatten (d : doc as {dc, id, ...}) =
      findWith (cache, id, fn () =>
        case dc
          of (Fail | Text _) => d
           | Newline NONE => fail
           | Newline (SOME s) => text s
           | Concat (a as { id = aId, ... }, b as { id = bId, ... }) =>
               let val ap as { id = aIdp, ... } = flatten a
                   val bp as { id = bIdp, ... } = flatten b
               in  if aIdp = aId andalso bIdp = bId then d else ap ^^ bp
               end
           | Choice (a as { id = aId, ... }, b as { id = bId, ... }) =>
               let val ap as { id = aIdp, ... } = flatten a
                   val bp as { id = bIdp, ... } = flatten b
               in  if aIdp = aId andalso bIdp = bId then d else ap <|> bp
               end
           | (Nest (_, d) | Align d | Reset d) => flatten d
           | Cost (c, d as { id, ... }) =>
               let val dp as { id = idp, ... } = flatten d
               in  if idp = id then d else cost c dp
               end
           | Blank _ => d)
  end

  fun op<+>(d1, d2) = d1 ^^ align d2
  fun group d = d <|> (flatten d)
  fun op<->(x, y) = (flatten x) <+> y
  fun foldDoc _ [] = empty
    | foldDoc f (x :: xs) = List.foldl f x xs
  val hcat = foldDoc (op<->)
  val vcat = foldDoc (op<$>)

  fun formatInfo (pw, d) =
    let val lines = DynamicArray.array (32, "")
        fun append line =
          DynamicArray.update (lines, DynamicArray.bound lines + 1, line)
        val info = solve append pw d
        fun finalize () = DynamicArray.foldr (op^) "" lines
    in  (finalize (), info)
    end
  fun pprint renderer pw d = ignore (solve renderer pw d)
  fun format pw d = #1 (formatInfo (pw, d))
  fun formatDebug pw d =
    let val (content, {isTainted, cost}) = formatInfo (pw, d)
    in  C.debugFormat pw (content, isTainted, C.toString cost)
    end
end

structure PrinterUtil :> sig
  val makeDebugFormat : int -> string * bool * string -> string
end = struct
  fun makeDebugFormat pagewidth (content, isTainted, cost) =
    let val lines = String.fields (fn c => c = #"\n") content
        val zeroCode = Char.ord #"0"
        val header = String.implode (List.tabulate (pagewidth,
          fn i => Char.chr ((i + 1) mod 10 + zeroCode)))
        val content =
          List.map (fn l =>
            if String.size l > pagewidth then
              String.substring (l, 0, pagewidth) ^ "|" ^
              String.substring (l, pagewidth, String.size l - pagewidth)
            else
              l ^ String.implode (List.tabulate
                                    (pagewidth - String.size l, fn _ => #" "))
                ^ "|")
            lines
        val content' = String.concatWith "\n" content
    in  header ^ "\n" ^
        content' ^ "\n" ^
        ("isTainted: " ^ Bool.toString isTainted) ^ "\n" ^
        ("cost: " ^ cost)
    end
end

structure DefaultCostModel :> COST_MODEL where type t = int * int =
struct
  type t = int * int
  fun limit pagewidth = Real.round (Real.fromInt pagewidth * 1.5)

  fun text (pagewidth, pos, len) =
    let val stop = pos + len
    in  if stop > pagewidth then
          let val maxwc = Int.max (pagewidth, pos)
              val a = maxwc - pagewidth
              val b = stop - maxwc
          in  (b * (2 * a + b), 0)
          end
        else
          (0, 0)
    end

  fun newline _ = (0, 1)

  fun combine ((o1, h1), (o2, h2)) = (o1 + o2, h1 + h2)

  fun le ((o1, h1), (o2, h2)) = if o1 = o2 then h1 <= h2 else o1 < o2

  fun toString (bad, h) = "(" ^ Int.toString bad ^ ", " ^ Int.toString h ^ ")"

  val debugFormat = PrinterUtil.makeDebugFormat
end
