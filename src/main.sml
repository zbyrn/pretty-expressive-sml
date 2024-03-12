structure Main = struct
  structure Cost = DefaultCostFn(val pagewidth = 24)
  structure Printer = PrinterFn(Cost)

  infix 4 ^^ <$> <-> <+>
  infix 1 <|>
  val realprint = print
  open Printer

  val d = text "while (true) {" ^^
          nest 4
            (nl ^^ text "f();" ^^ nl ^^ text "if (done())" ^^
             (let val exitD = text "exit();"
              in  (space ^^ exitD) <|> nest 4 (nl ^^ exitD)
              end)) ^^
          nl ^^ text "}"

  fun go () = (print realprint d)
end
