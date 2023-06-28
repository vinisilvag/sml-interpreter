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
    | ESeq seqType => SeqV []
    | Var x => lookup env x
    | Let(x, e1, e2) =>
      let
        val v1 = eval e1 env
        val env' = (x, v1) :: env
      in
        eval e2 env'
      end
    | Letrec(f, pt, p, rt, e1, e2) =>
      let
        val env' = (f, Clos(f, p, e1, env)) :: env
      in
        eval e2 env'
      end
    | Prim1(opr, e1) =>
      let
        val v1 = eval e1 env
      in
        case (opr, v1) of
            ("!", BoolV b) => BoolV (not b)
          | ("-", IntV i) => IntV (~i)
          | ("hd", SeqV []) => raise HDEmptySeq
          | ("hd", SeqV (h::t)) => h
          | ("tl", SeqV []) => raise TLEmptySeq
          | ("tl", SeqV (h::[])) => raise TLEmptySeq
          | ("tl", SeqV (h::t)) => SeqV t
          | ("ise", SeqV []) => BoolV (true)
          | ("ise", SeqV (h::t)) => BoolV (false)
          | ("print", _) =>
            let
              val str = val2string(v1)
              val print = print(str ^ "\n")
            in
              ListV []
            end
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
        case cv of
            (BoolV true) => eval t env
          | (BoolV false) => eval e env
          | _ => raise Impossible
      end 
    (* | Match(x, ml) => raise NotImplemented *)
    | Call(f, e) => (* not sure if it's right *)
      let
        val fv = eval f env
      in
        case fv of
            Clos(fname, p, fe, fenv) =>
              let
                val ev = eval e env
                val env' = (p, ev) :: (fname, fv) :: env
              in
                eval fe env'
              end
          | _ => raise NotAFunc
      end
    | List(el) =>
      let
        fun valList l = 
          case l of
              [] => []
            | (h::t) => eval h env :: (valList t)
      in
        ListV (valList el)
      end
    | Item(idx, e) =>
      let
        val v = eval e env
      in
        case v of
            ListV vl =>
              let
                fun findIndexVal idx l =
                  case l of
                      [] => raise Impossible
                    | (h::t) =>
                      if idx = 1 then
                        h
                      else
                        findIndexVal (idx - 1) t
              in
                findIndexVal idx vl
              end
          | _ => raise Impossible
      end
    | Anon(pt, p, e) => Clos("", p, e, env)
    | _ => raise Impossible
