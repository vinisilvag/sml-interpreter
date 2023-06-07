(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(* temporary exception *)
exception NotImplemented

fun eval (e: expr) (env: plcVal env): plcVal = 
  case e of
      ConI i => IntV i
    | ConB b => BoolV b
    | ESeq seqType => raise NotImplemented
    | Var x => lookup env x
    | Let(x, e1, e2) =>
      let
        val v1 = eval e1 env
        val env' = (x, v1) :: env
      in
        eval e2 env'
      end
    | Letrec(f, pt, p, rt, e1, e2) => raise NotImplemented
    | Prim1(opr, e1) =>
      let
        val v1 = eval e1 env
      in
        case opr of
            ("!") => raise NotImplemented
          | ("-") => raise NotImplemented
          | ("hd") => raise NotImplemented
          | ("tl") => raise NotImplemented
          | ("ise") => raise NotImplemented
          | ("print") => raise NotImplemented
          | _ => raise NotImplemented
      end
    | Prim2(opr, e1, e2) =>
      let
        val v1 = eval e1 env
        val v2 = eval e2 env
      in
        case opr of
            ("&&") => raise NotImplemented
          | ("+") => raise NotImplemented
          | ("-") => raise NotImplemented
          | ("*") => raise NotImplemented
          | ("/") => raise NotImplemented
          | ("=") => raise NotImplemented
          | ("!=") => raise NotImplemented
          | ("<") => raise NotImplemented
          | ("<=") => raise NotImplemented
          | ("::") => raise NotImplemented
          | (";") => raise NotImplemented
          | ("[") => raise NotImplemented
          | _ => raise NotImplemented
      end
    | If(c, t, e) =>
      let
        val cv = eval c env
      in
        if cv then eval t env else eval e env
      end 
    (* | Match => raise NotImplemented
    | Call => raise NotImplemented *)
    | List(el) => raise NotImplemented
    | Item(idx, e) => raise NotImplemented
    | Anon(pt, p, e) => raise NotImplemented
    | _ => raise NotImplemented