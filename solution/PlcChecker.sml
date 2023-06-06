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

fun lookupType [] id = raise UnknownType
  | lookupType ((k: string, t) :: tl) id = if k = id then t else lookupType tl id;

fun teval (e: expr) (env: plcType env): plcType = 
  case e of
      ConI _ => IntT
    | ConB _ => BoolT
    | ESeq t' => 
        case t' of
            (SeqT t) => t
          | _ => raise EmptySeq
    | Var x => lookupType x env
    | Let(v, e1, e2) =>
      let
        val t = teval e1 env
        val env' = (v, t) :: env
      in
        teval e2 env'
      end
    (* | Letrec(f, ft, p, pt, e1, e2) => ? (e1 e e2 são o que?) *)
    | Prim1(opr, e1) =>
      let
        val t1 = teval e1 env
      in
        case (opr, t1) of
            ("!", BoolT) => BoolT
          | ("!", _) => raise ?
          | ("-") => ?
          | ("hd") => ?
          | ("tl") => ?
          | ("ise") => ?
          | ("print") => ?
      end
    | Prim2(opr, e1, e2) =>
      let
        val t1 = teval e1 env;
        val t2 = teval e2 env
      in
        case opr of
            ("&&") => ?
          | ("+") => ?
          | ("-") => ?
          | ("*") => ?
          | ("/") => ?
          | ("=") => ?
          | ("!=") => ?
          | ("<") => ?
          | ("<=") => ?
          | ("::") => ?
          | (";") => ?
          (* | ("[") => ? *)
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
    (* | Match *)
    (* | Call *)
    (* | List *)
    (* | Item *)
    (* | Anon *)
    | _ => raise UnknownType;