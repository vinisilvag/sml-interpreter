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
        val str = val2string v1
      in
        case (opr, v1) of
            ("!", BoolV b) => BoolV (not b)
          | ("-", IntV i) => IntV (~ i)
          | ("hd", _) => raise NotImplemented
          | ("tl", _) => raise NotImplemented
          | ("ise", _) => raise NotImplemented
          | ("print", _) => raise NotImplemented
          | _ => raise Impossible
      end
    | Prim2(opr, e1, e2) =>
      let
        val v1 = eval e1 env
        val v2 = eval e2 env
      in
        case (opr, v1, v2) of
            ("&&", BoolV b1, BoolV b2) => BoolV (b1 andalso b2)
          | ("+", IntV i1, IntV i2) => IntV (i1 + i2)
          | ("-", IntV i1, IntV i2) => IntV (i1 - i2)
          | ("*", IntV i1, IntV i2) => IntV (i1 * i2)
          | ("/", IntV i1, IntV i2) => IntV (i1 div i2)
          | ("=", IntV i1, IntV i2) => if i1 = i2 then BoolV (true) else BoolV (false)
          | ("=", BoolV b1, BoolV b2) => if b1 = b2 then BoolV (true) else BoolV (false)
          | ("=", _, _) => raise NotImplemented
          | ("!=", IntV i1, IntV i2) => if i1 <> i2 then BoolV (true) else BoolV (false)
          | ("!=", BoolV b1, BoolV b2) => if b1 <> b2 then BoolV (true) else BoolV (false)
          | ("!=", _, _) => raise NotImplemented
          | ("<", IntV i1, IntV i2) => if i1 < i2 then BoolV (true) else BoolV (false)
          | ("<=", IntV i1, IntV i2) => if i1 <= i2 then BoolV (true) else BoolV (false)
          | ("::", _, _) => raise NotImplemented
          | (";", _, _) => raise NotImplemented
          | _ => raise Impossible
      end
    | If(c, t, e) =>
      let
        val cv = eval c env
      in
        if cv = BoolV (true) then eval t env else eval e env
      end 
    (* | Match => raise NotImplemented *)
    | Call(f, e) => raise NotImplemented
    | List(el) => raise NotImplemented
    | Item(idx, e) => raise NotImplemented
    | Anon(pt, p, e) => raise NotImplemented
    | _ => raise NotImplemented