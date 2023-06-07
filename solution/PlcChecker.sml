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

fun isEqualityType t =
  case t of
      IntT => true
    | BoolT => true
    | ListT [] => true
    | (SeqT seqType) => isEqualityType seqType 
    | _ => false

fun teval (e: expr) (env: plcType env): plcType = 
  case e of
      ConI _ => IntT
    | ConB _ => BoolT
    | ESeq seqType => (
      case seqType of
          (SeqT t) => SeqT t (* t or SeqT t? *)
        | _ => raise EmptySeq
    )
    | Var x => lookup env x
    | Let(x, e1, e2) =>
      let
        val t = teval e1 env
        val env' = (x, t) :: env
      in
        teval e2 env'
      end
    | Letrec(f, pt, p, rt, e1, e2) => raise NotImplemented
    | Prim1(opr, e1) =>
      let
        val t1 = teval e1 env
      in
        case opr of
            ("!") => if t1 = BoolT then BoolT else raise UnknownType
          | ("-") => if t1 = IntT then IntT else raise UnknownType
          | ("hd") => (
            case t1 of
                (SeqT t) => t
              | _ => raise OpNonList
          )
          | ("tl") => (
            case t1 of
                (SeqT t) => SeqT t
              | _ => raise OpNonList
          )
          | ("ise") => (
            case t1 of
                (SeqT t) => BoolT
              | _ => raise OpNonList
          )
          | ("print") => ListT []
          | _ => raise UnknownType
      end
    | Prim2(opr, e1, e2) =>
      let
        val t1 = teval e1 env
        val t2 = teval e2 env
      in
        case opr of
            ("&&") => if t1 = BoolT andalso t2 = BoolT then BoolT else raise UnknownType
          | ("+") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("-") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("*") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("/") => if t1 = IntT andalso t2 = IntT then IntT else raise UnknownType
          | ("=") =>
            if t1 = t2 andalso isEqualityType t1 then
              BoolT
            else
              raise NotEqTypes
          | ("!=") =>
            if t1 = t2 andalso isEqualityType t1 then
              BoolT
            else
              raise NotEqTypes
          | ("<") => if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
          | ("<=") => if t1 = IntT andalso t2 = IntT then BoolT else raise UnknownType
          | ("::") => raise NotImplemented
          | (";") => t2
          | ("[") => raise NotImplemented
          | _ => raise UnknownType
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
    | Item(idx, e) => 
      let
        val t = teval e env
      in
        case t of
            ListT tl =>
              let
                fun findIndexType idx [] = raise ListOutOfRange
                  | findIndexType idx (h::t) =
                      if idx = 1 then
                        h
                      else
                        findIndexType (idx - 1) t
              in
                findIndexType idx tl
              end
          | _ => raise OpNonList
      end
    | Anon(pt, p, e) => raise NotImplemented
    | _ => raise UnknownType
