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

fun listLength [] = 0
  | listLength (h::t) = 1 + (listLength t);

fun indexOnBound idx listLength =
  (idx > 0 andalso idx <= listLength)

fun isEqualityType t =
  case t of
      IntT => true
    | BoolT => true
    | ListT listType => 
        let
          fun sameTypeList l = 
            case l of
                [] => true
              | (h::t) => (isEqualityType h) andalso (sameTypeList t)
        in
          sameTypeList listType
        end
    | (SeqT seqType) => isEqualityType seqType
    | _ => false

fun teval (e: expr) (env: plcType env): plcType = 
  case e of
      ConI _ => IntT
    | ConB _ => BoolT
    | ESeq(SeqT t) => SeqT t
    | ESeq _ => raise EmptySeq
    | Var x => lookup env x
    | Let(x, e1, e2) =>
        let
          val t = teval e1 env
          val env' = (x, t) :: env
        in
          teval e2 env'
        end
    | Letrec(f, pt, p, rt, e1, e2) =>
        let
          val rEnv = (p, pt) :: (f, FunT(pt, rt)) :: env
          val cEnv = (f, FunT(pt, rt)) :: env
          val t1 = teval e1 rEnv
        in
          if t1 = rt then
            teval e2 cEnv
          else
            raise WrongRetType
        end
    | Prim1(opr, e1) =>
        let
          val t1 = teval e1 env
        in
          case (opr, t1) of
              ("!", BoolT) => BoolT
            | ("-", IntT) => IntT
            | ("hd", SeqT t) => t
            | ("tl", SeqT t) => SeqT t
            | ("ise", SeqT _) => BoolT
            | ("print", _) => ListT []
            | _ => raise UnknownType
        end
    | Prim2(opr, e1, e2) =>
        let
          val t1 = teval e1 env
          val t2 = teval e2 env
        in
          case (opr, t1, t2) of
              ("&&", BoolT, BoolT) => BoolT
            | ("+", IntT, IntT) => IntT
            | ("-", IntT, IntT) => IntT
            | ("*", IntT, IntT) => IntT
            | ("/", IntT, IntT) => IntT
            | ("=", t1, t2) =>
              if t1 = t2 andalso isEqualityType t1 then
                BoolT
              else
                raise NotEqTypes
            | ("!=", t1, t2) =>
              if t1 = t2 andalso isEqualityType t1 then
                BoolT
              else
                raise NotEqTypes
            | ("<", IntT, IntT) => BoolT
            | ("<=", IntT, IntT) => BoolT
            | ("::", t1, t2) => ( (* maybe not fully implemented yet *)
                case (t1, t2) of
                    (IntT, SeqT(IntT)) => SeqT(IntT)
                  | (BoolT, SeqT(BoolT)) => SeqT(BoolT)
                  | _ => raise NotEqTypes
              )
            | (";", _, _) => t2
            | _ => raise UnknownType
        end
    | If(c, t, e) =>
        let
          val ct = teval c env
        in
          case ct of
              BoolT =>
                let
                  val tt = teval t env
                  val et = teval e env
                in
                  if tt = et then tt else raise DiffBrTypes
                end
            | _ => raise IfCondNotBool
        end
    (* | Match(x, ml) => raise NotImplemented *)
    | Call(f, e) =>
        let
          val ft = teval f env
        in
          case ft of
              FunT(pt, rt) =>
                let
                  val et = teval e env
                in
                  if pt = et then rt else raise CallTypeMisM
                end 
            | _ => raise NotFunc
        end
    | List(el) =>
        let
          fun typeList l = 
            case l of
                [] => []
              | (h::t) => (teval h env) :: (typeList t)
        in
          ListT(typeList el)
        end
    | Item(idx, e) => 
        let
          val t = teval e env
        in
          case t of
              ListT tl =>
                let
                  fun findIndexType idx l =
                    case l of
                        [] => raise ListOutOfRange
                      | (h::t) =>
                        if idx = 1 then
                          h
                        else
                          findIndexType (idx - 1) t
                in
                  findIndexType idx tl
                end
            | _ => raise OpNonList
        end
    | Anon(pt, p, e) =>
        let
          val env' = (p, pt) :: env
          val rt = teval e env'
        in
          FunT(pt, rt)
        end
    | _ => raise UnknownType
