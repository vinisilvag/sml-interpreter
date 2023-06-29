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
            | ("=", _, _) =>
                let
                  val eqt1 = isEqualityType t1
                  val eqt2 = isEqualityType t2
                in
                  if (t1 = t2) andalso eqt1 andalso eqt2 then
                    BoolT
                  else
                    raise NotEqTypes
                end
            | ("!=", _, _) =>
                let
                  val eqt1 = isEqualityType t1
                  val eqt2 = isEqualityType t2
                in
                  if (t1 = t2) andalso eqt1 andalso eqt2 then
                    BoolT
                  else
                    raise NotEqTypes
                end
            | ("<", IntT, IntT) => BoolT
            | ("<=", IntT, IntT) => BoolT
            | ("::", _, SeqT t) =>
                if t1 = t then
                  SeqT t
                else
                  raise NotEqTypes
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
    | Match(x, ml) =>
        let
          fun sameCondType typ l =
            case l of
                [] => true
              | (SOME e, _)::l' => (teval x env = typ) andalso (sameCondType typ l')
              | (NONE, _)::l' => sameCondType typ l'
          fun sameResType typ l =
            case l of
                [] => true
              | (_, e)::l' => (teval e env = typ) andalso (sameResType typ l')
        in
          if sameCondType (teval x env) ml then
            case ml of
                [] => raise NoMatchResults
              | (_, e1)::l' => 
                  let
                    val resType = teval e1 env
                  in
                    if sameResType resType l' then
                      resType
                    else
                      raise MatchResTypeDiff
                  end
          else
            raise MatchCondTypesDiff
        end
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
          ListT (typeList el)
        end
    | Item(idx, e) => 
        let
          val t = teval e env
        in
          case t of
              ListT tl =>
                if (indexOnBound idx (List.length tl)) then
                  List.nth(tl, idx - 1)
                else
                  raise ListOutOfRange
            | _ => raise OpNonList
        end
    | Anon(pt, p, e) =>
        let
          val env' = (p, pt) :: env
          val rt = teval e env'
        in
          FunT(pt, rt)
        end
