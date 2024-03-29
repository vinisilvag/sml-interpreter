(* Plc interpreter main file *)

fun run (e: expr) =
  let
    val et = teval e []
    val ev = eval e []
  in
    (val2string ev) ^ " : " ^ (type2string et)
  end
  handle SymbolNotFound => "Symbol not found."
    | EmptySeq => "Plc Checker: Empty sequence should have a sequence type."
    | UnknownType => "Plc Checker: Unknown operator, or type error."
    | NotEqTypes => "Plc Checker: Types in comparison are different."
    | WrongRetType => "Plc Checker: Wrong return type in function."
    | DiffBrTypes => "Plc Checker: 'if' branch types differ."
    | IfCondNotBool => "Plc Checker: 'if' condition not Boolean."
    | NoMatchResults => "Plc Checker: no Match results."
    | MatchResTypeDiff => "Plc Checker: 'match' result types differ."
    | MatchCondTypesDiff => "Plc Checker: 'match' condition types differ matching expressions's type."
    | CallTypeMisM => "Plc Checker: Type mismatch in function call."
    | NotFunc => "teval Call: not a function."
    | ListOutOfRange => "Plc Checker: List index out of range."
    | OpNonList => "Plc Checker: Selection with operator # applied to non-list."
    | Impossible => "Plc Interp: Impossible evalulate expression."
    | HDEmptySeq => "Plc Interp: 'hd' empty sequence argument."
    | TLEmptySeq => "Plc Interp: 'tl' empty sequence argument."
    | ValueNotFoundInMatch => "Plc Interp: value not found in match."
    | NotAFunc => "Plc Interp: eval Call: not a function.";
