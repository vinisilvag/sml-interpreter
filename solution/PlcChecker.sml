(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

(* temporary exception *)
exception NotImplemented

fun teval (e: expr) (env: plcType env): plcType = 
  case e of
      ConI _ => IntT
    | ConB _ => BoolT
    | ESeq seqType =>
      let in
        case seqType of
            (SeqT t) => SeqT t (* t or SeqT t? *)
          | _ => raise EmptySeq
      end
    | Var x => lookup env x
    | Let(x, e1, e2) =>
      let
        val t = teval e1 env
        val env' = (x, t) :: env
      in
        teval e2 env'
      end
    | Letrec(f, ft, p, pt, e1, e2) => raise NotImplemented
    | Prim1(opr, e1) =>
      let
        val t1 = teval e1 env
      in
        case opr of
            ("!") => if t1 = BoolT then BoolT else raise UnknownType
          | ("-") => if t1 = IntT then IntT else raise UnknownType
          | ("hd") => raise NotImplemented
          | ("tl") => raise NotImplemented
          | ("ise") => raise NotImplemented
          | ("print") => raise NotImplemented
      end
    | Prim2(opr, e1, e2) =>
      let
        val t1 = teval e1 env;
        val t2 = teval e2 env
      in
        case opr of
            ("&&") => if t1 = BoolT andalso t2 = BoolT then BoolT else raise UnknownType
          | ("+") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("-") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("*") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("/") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("=") => raise NotImplemented
          | ("!=") => raise NotImplemented
          | ("<") => if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
          | ("<=") => if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
          | ("::") => raise NotImplemented
          | (";") => t2
          | ("[") => raise NotImplemented
      end
    | If(c, t, e) =>
      let
        val ct = teval c env
        val tt = teval t env
        val et = teval e env
      in
        case ct of
            BoolT => if tt = et then tt else raise DiffBrTypes
          | _ => raise IfCondNotBool
      end
    (* | Match => raise NotImplemented
    | Call => raise NotImplemented *)
    | List(el) =>
      let
        fun typeList [] = []
          | typeList (h::t) = teval h env :: (typeList t)
      in
        ListT (typeList el)
      end
    | Item(idx, e) => raise NotImplemented
    | Anon(pt, p, e) => raise NotImplemented
    | _ => raise UnknownType;