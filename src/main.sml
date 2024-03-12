structure Main = struct
  structure Printer = PrinterFn(DefaultCostModel)

  infix 4 ^^ <$> <-> <+>
  infix 1 <|>
  val realprint = print
  open Printer

  val d = text "while (true) {" ^^
          nest 4
            (nl ^^ text "f();" ^^ nl ^^ text "if (done())" ^^
             (let val exitD = text "exit();"
              in  (space ^^ exitD)
                  <|> (space ^^ lbrace ^^ nest 4 (nl ^^ exitD) ^^ nl ^^ rbrace)
              end)) ^^
          nl ^^ text "}"

  fun go pagewidth = realprint (formatDebug pagewidth d)
end
