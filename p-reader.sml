open Sparcl

datatype Index = Index of int

fun scanString cvt s =
  let
    val len = String.size s

    fun rdr (Index i) =
      if i = len
      then NONE
      else SOME (String.sub(s, i), Index(i+1))
  in
    case cvt rdr (Index 0) of
         Done (r, (Index i)) => Done (r, String.extract(s, i, NONE))
       | Partial             => Partial
       | Fail                => Fail
  end

fun scanSubstring cvt s =
  let
    val len = Substring.size s

    fun rdr (Index i) =
      if i = len
      then NONE
      else SOME (Substring.sub(s, i), Index(i+1))
  in
    case cvt rdr (Index 0) of
         Done (r, (Index i)) => Done (r, Substring.triml i s )
       | Partial             => Partial
       | Fail                => Fail
  end


(*
local
  fun showResult r = case r of
      Done(s, t) => print ("Done: " ^ s ^ ", tail: " ^ t ^ "\n")
    | Partial => print ("Partial\n") | Fail => print ("Fail\n")
in

  val r = scanString (takeStr "--") "--abcde-=*xyz"
  val _ = showResult r

  val r = scanString (takeBefore "-=*") "abcde-=*xyz"
  val _ = showResult r

  val r = scanString (takeStr "-=*") "-=*xyz"
  val _ = showResult r

  val r = scanString takeTail "xyz"
  val _ = showResult r

  val _ = print "----\n\n"

end

val parseArg = (takeStr "--") *> (takeBefore "-=*") >>= (fn k => (takeStr "-=*") *> takeTail >>= (fn v => pure (k, v)))
val r = scanString parseArg "--abcde-=*xyz"
val _ = case r of Done((k, v), t) => print ("SOME '" ^ k ^ "' is '" ^ v ^ "'\n") | Partial => print ("Partial\n") | Fail => print ("Fail\n")
*)


(* http://stackoverflow.com/questions/14750444/how-can-i-parse-string-to-int-int-tuple-in-sml *)
val scanLine = takeInt >>= (fn x => takeStr "," *> takeInt >>= (fn y => takeStr "\n" *> pure (x, y) ) )
val scanList = many scanLine

(*
fun showListPair []         = ()
  | showListPair ((x,y)::t) = ( print ((Int.toString x) ^ " " ^ (Int.toString y) ^ "\n") ; showListPair t )

val r = scanString scanList "4,5\n2,3\n-"
val _ = case r of Done (l, t) => ( print ("Done\n") ; showListPair l ) | Partial => print ("Partial\n") | Fail => print ("Fail\n")
*)



val scanRedis = takeStr "$" *> takeInt >>= (fn n => takeStr "\r\n" *> takeN n <* takeStr "\r\n")

(*
val r = scanString scanRedis "$4\r\nINFO\r\nTAIL"
val _ = case r of Done (cmd, t) => print ("Done " ^ cmd ^ "\n") | Partial => print ("Partial\n") | Fail => print ("Fail\n")

val r = scanSubstring scanRedis (Substring.full "$4\r\nINFO\r\nTAIL")
val _ = case r of Done (cmd, t) => print ("Done " ^ cmd ^ "\n") | Partial => print ("Partial\n") | Fail => print ("Fail\n")
*)


fun runBench name n f s =
let
  fun loop 0 = ()
    | loop i = (f s; loop (i - 1))

  val t0 = Time.now ()
  val _ = loop n
  val t1 = Time.now ()
in
  print (name ^ " " ^ Real.toString(Time.toReal(Time.-(t1, t0))) ^ "\n")
end


val N = 10000000
val sfull = Substring.full

fun main () = (
  print "Run Benckmark...\n";
  runBench "Bench scanRedis, String   " N (scanString scanRedis)           "$4\r\nINFO\r\nTAIL";
  runBench "Bench CSV,       String   " N (scanString scanList)            "4,5\n2,3\n-";
  runBench "Bench scanRedis, Substring" N (scanSubstring scanRedis) (sfull "$4\r\nINFO\r\nTAIL");
  runBench "Bench CSV,       Substring" N (scanSubstring scanList)  (sfull "4,5\n2,3\n-")
  )
