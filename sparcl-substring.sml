infix 4 <$>
infix 1 >>=
infix 4 *>
infix 4 <*
infix 4 <*>


signature SPARCL = sig
  datatype 'r Result = Done of 'r * substring | Partial | Fail
      type 'r Parser = substring -> 'r Result

  val takeStr    : string -> substring Parser
  val takeBefore : string -> substring Parser
  val takeInt    :                 int Parser
  val takeN      : int    -> substring Parser

  val pure   : 'r -> 'r Parser
  val fmap   : ('r1 -> 'r2)        -> 'r1 Parser          -> 'r2 Parser
  val bind   : 'r1 Parser          -> ('r1 -> 'r2 Parser) -> 'r2 Parser
  val apR    : 'r1 Parser          -> 'r2 Parser          -> 'r2 Parser
  val apL    : 'r1 Parser          -> 'r2 Parser          -> 'r1 Parser
  val ap     : ('r1 -> 'r2) Parser -> 'r1 Parser          -> 'r2 Parser
  val choice : 'r  Parser list     -> 'r  Parser
  val many   : 'r Parser -> 'r list Parser
  
  val <$>    : ('r1 -> 'r2)    *  'r1 Parser          -> 'r2 Parser
  val >>=    : 'r1 Parser      *  ('r1 -> 'r2 Parser) -> 'r2 Parser
  val  *>    : 'r1 Parser      *  'r2 Parser          -> 'r2 Parser
  val <*     : 'r1 Parser      *  'r2 Parser          -> 'r1 Parser
  val <*>    : ('r1 -> 'r2) Parser * 'r1 Parser       -> 'r2 Parser
end


structure Sparcl : SPARCL = struct
  datatype 'r Result = Done of 'r * substring | Partial | Fail
      type 'r Parser = substring -> 'r Result

  fun takeStr s = 
    let 
      val s_l = String.size s
      val s_s = Substring.full s
    in
      fn ss => 
        let
          val ss_l = Substring.size ss
        in
          if ss_l >= s_l
          then 
              let
                val sss = ( Substring.slice (ss, 0, SOME s_l))
              in
                if Substring.compare(sss, s_s) = EQUAL
                then Done (s_s, (Substring.triml s_l ss))
                else Fail 
              end
          else Partial
        end
    end

  fun takeBefore s =
    let
        val s_l = String.size s
    in
      fn ss => 
        let
          val (h,t) = Substring.position s ss
        in
          if Substring.isEmpty t
          then Partial
          else Done ((h), t)
        end
    end

  val takeInt = fn ss =>
    if Substring.isEmpty ss
    then Partial
    else
      let
        val sd = Substring.takel (fn c => Char.isDigit c orelse c = #"-" orelse c = #"+") ss
      in
        if Substring.isEmpty sd
        then Fail
        else
          let 
            val sd_l = Substring.size sd
            val tail = Substring.triml sd_l ss
            val d = Int.fromString (Substring.string sd)
          in
            if Substring.isEmpty tail 
            then Partial
            else case d of
                     SOME d => Done (d, tail)
                   | NOME   => Fail
          end
      end 

  fun takeN s_l = fn ss =>
    if Substring.size ss >= s_l
    then
      let
        val sss = ( Substring.slice (ss, 0, SOME s_l))
      in
        Done (sss, (Substring.triml s_l ss))
      end
    else Partial 
 
  (* combinators *)

  fun pure s = fn ss => Done (s, ss)

  fun fmap f p = fn ss =>
    case p ss of
        Done (r, t) => Done ((f r), t)
      | Partial     => Partial
      | Fail        => Fail

  fun bind p pg = fn ss =>
    case p ss of
        Done (r, t) => (pg r) t
      | Partial     => Partial
      | Fail        => Fail

  fun apR p1 p2 = fn ss => 
    case p1 ss of 
        Done (r1, t1) => p2 t1
      | Partial       => Partial
      | Fail          => Fail

  fun apL p1 p2 = fn ss => 
    case p1 ss of
        Done (r1, t1) => (case p2 t1 of
                              Done (r2, t2) => Done (r1, t2)
                            | Partial  => Partial
                            | Fail     => Fail)
      | Partial  => Partial
      | Fail     => Fail

  fun ap p1 p2 = fn ss =>
    case p1 ss of
        Done (r1, t1) => (case p2 t1 of
                              Done (r2, t2) => Done ((r1 r2), t2)
                            | Partial  => Partial
                            | Fail     => Fail)
      | Partial  => Partial
      | Fail     => Fail

  fun choice ps = fn ss => 
    let
     fun go nil true  = Partial
       | go nil false = Fail
       | go (p::ps) is_Partial =
          case p ss of
              Done (r, t) => Done (r, t)
            | Partial     => go ps true 
            | Fail        => go ps is_Partial 
    in go ps false end

  fun many p ss =
    case p ss of
        Done (r, t) => ( case many p t of
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
