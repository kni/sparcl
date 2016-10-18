infix 4 <$>
infix 1 >>=
infix 4 *>
infix 4 <*
infix 4 <*>


signature SPARCL = sig

  datatype ('r, 'cs) Result = Done of 'r * 'cs | Partial | Fail
      type ('r, 'cs) Parser = (char, 'cs) StringCvt.reader -> 'cs -> ('r, 'cs) Result

  val takeStr    : string -> (string, 'cs) Parser
  val takeBefore : string -> (string, 'cs) Parser
  val takeInt    : (int, 'cs) Parser
  val takeN      : int -> (string, 'cs) Parser
  val takeTail   : (string, 'cs) Parser

  val pure   : 'a -> ('a, 'cs) Parser
  val fmap   : ('a -> 'b) -> ('a, 'cs) Parser -> ('b, 'cs) Parser
  val bind   : ('a, 'cs) Parser -> ('a -> ('b, 'cs) Parser) -> ('b, 'cs) Parser
  val apR    : ('a, 'cs) Parser -> ('b, 'cs) Parser -> ('b, 'cs) Parser
  val apL    : ('a, 'cs) Parser -> ('b, 'cs) Parser -> ('a, 'cs) Parser
  val ap     : (('a -> 'b), 'cs) Parser -> ('a, 'cs) Parser -> ('b, 'cs) Parser
  val choice : ('a, 'cs) Parser list -> ('a, 'cs) Parser
  val many   : ('a, 'cs) Parser -> ('a list, 'cs) Parser

  val <$> : ('a -> 'b) * ('a, 'cs) Parser -> ('b, 'cs) Parser
  val >>= : ('a, 'cs) Parser * ('a -> ('b, 'cs) Parser) -> ('b, 'cs) Parser
  val  *> : ('a, 'cs) Parser * ('b, 'cs) Parser -> ('b, 'cs) Parser
  val <*  : ('a, 'cs) Parser * ('b, 'cs) Parser -> ('a, 'cs) Parser
  val <*> : (('a -> 'b), 'cs) Parser * ('a, 'cs) Parser -> ('b, 'cs) Parser
end


structure Sparcl : SPARCL = struct

  datatype ('r, 'cs) Result = Done of 'r * 'cs | Partial | Fail
      type ('r, 'cs) Parser = (char, 'cs) StringCvt.reader -> 'cs -> ('r, 'cs) Result

  local

  (* compare and return if match *)
  fun compare s []     getc strm = Done (s, strm)
    | compare s (h::t) getc strm =
        case getc strm of
             NONE           => Partial 
           | SOME (c, strm) =>
               if c = h
               then compare s t getc strm
               else Fail

  in

  fun takeStr s =
    let
      val e = String.explode s
    in
      compare s e
    end

  fun takeBefore s =
    let
      val e = String.explode s

      fun scan r getc strm =
        case compare s e getc strm of
             Done (_, _) => Done (String.implode(List.rev r), strm)
           | Partial     => Partial
           | Fail        =>
                case getc strm of
                    NONE => Partial
                  | SOME (c, strm) => scan (c::r) getc strm

    in
      scan []
    end

  fun takeInt getc strm =
    case Int.scan StringCvt.DEC getc strm of
        NONE           => Fail
      | SOME (i, strm) => case getc strm of NONE => Partial | SOME (_, _) => Done (i, strm)
      


  fun takeN n getc strm =
    let
      fun doit 0 l strm = Done (String.implode(List.rev l), strm)
        | doit i l strm =
          case getc strm of
              NONE => Partial
            | SOME (c, strm) => doit (i - 1) (c::l) strm
    in
      doit n [] strm
    end


  fun takeTail getc strm =
    let
      fun scan r getc strm =
          case getc strm of
              NONE => Done (String.implode(List.rev r), strm)
            | SOME (c, strm) => scan (c::r) getc strm
    in
      scan [] getc strm
    end




  fun pure s getc strm = Done (s, strm)

  fun fmap f p = fn getc => fn strm =>
    case p getc strm of
        Done (r, t) => Done ((f r), t)
      | Partial     => Partial
      | Fail        => Fail

  fun bind p pg = fn getc => fn strm =>
    case p getc strm of
        Done (r, t) => (pg r) getc t
      | Partial     => Partial
      | Fail        => Fail

  fun apR p1 p2 = fn getc => fn strm =>
    case p1 getc strm of
        Done (r1, t1) => p2 getc t1
      | Partial       => Partial
      | Fail          => Fail

  fun apL p1 p2 = fn getc => fn strm =>
    case p1 getc strm of
        Done (r1, t1) => (case p2 getc t1 of
                              Done (r2, t2) => Done (r1, t2)
                            | Partial  => Partial
                            | Fail     => Fail)
      | Partial  => Partial
      | Fail     => Fail

  fun ap p1 p2 = fn getc => fn strm =>
    case p1 getc strm of
        Done (r1, t1) => (case p2 getc t1 of
                              Done (r2, t2) => Done ((r1 r2), t2)
                            | Partial  => Partial
                            | Fail     => Fail)
      | Partial  => Partial
      | Fail     => Fail

  fun choice ps = fn getc => fn strm =>
    let
     fun go nil true  = Partial
       | go nil false = Fail
       | go (p::ps) is_Partial =
          case p getc strm of
              Done (r, t) => Done (r, t)
            | Partial     => go ps true 
            | Fail        => go ps is_Partial 
    in go ps false end

  fun many p = fn getc => fn strm =>
    case p getc strm of
        Done (r, t) => ( case many p getc t of
                            Done (rs, t) => Done (r::rs, t)
                          | _            => Done (r::[], t)
                       )
      | Partial     => Partial
      | Fail        => Fail

  fun f  <$> p  = fmap f p
  fun p  >>= pg = bind p pg
  fun p1  *> p2 = apR p1 p2
  fun p1 <*  p2 = apL p1 p2
  fun p1 <*> p2 = ap  p1 p2

  end

end
