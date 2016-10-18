open Sparcl


fun resultShow (Done (r, t)) = print (r ^ ", " ^ (Substring.string t) ^ "\n")
  | resultShow Partial       = print "Partial\n"
  | resultShow Fail          = print "Fail\n"


fun compareResult (Done (r1, t1)) (Done (r2, t2)) = r1 = r2 andalso Substring.compare(t1, t2) = EQUAL
  | compareResult Partial         Partial         = true
  | compareResult Fail            Fail            = true
  | compareResult _               _               = false


fun testResult r expected name =
  if compareResult r expected
  then print ("OK - " ^ name ^ "\n" )
  else print ("Not OK - " ^ name ^ "\n" )



val sfull = Substring.full



val foo = (
    bind
        (apR (takeStr "$") takeInt )
        (fn n =>
            (apR (takeStr "\r\n")
                (apL (takeN n) (takeStr "\r\n"))
            )
        )
    )

val foo_infix =
        takeStr "$" *> takeInt
        >>=
        (fn n => takeStr "\r\n" *> takeN n <* takeStr "\r\n")


val scanLine = takeInt >>= (fn x => (takeStr ",") *> takeInt >>= (fn y => (takeStr "\n") *> pure (x, y) ) )
val scanList = many scanLine

(*
val r = scanList (sfull "4,5\n2,3\n-")

fun showListPair []         = ()
  | showListPair ((x,y)::t) = ( print ((Int.toString x) ^ " " ^ (Int.toString y) ^ "\n") ; showListPair t )

val _ = case r of Fail => print "Fail\n" | Partial => print "Partial\n" |
  Done (l, t) => ( print "Done\n"; showListPair l; print ("TAIL: " ^ (Substring.string t) ^ "\n") )
*)


fun sample () = (
  testResult
    ((takeStr "INFO") (sfull "INFOTAIL"))
    (Done ("INFO", sfull "TAIL"))
    "takeStr" ;

  testResult ( takeStr    "INFO" (sfull "INFOTAIL") ) (Done ("INFO", sfull "TAIL")) "takeStr" ;
  testResult ( takeBefore "TAIL" (sfull "INFOTAIL") ) (Done ("INFO", sfull "TAIL")) "takeBefore" ;

  testResult ( foo       (sfull "$4\r\nINFO\r\nTAIL") ) (Done ("INFO", sfull "TAIL")) "foo" ;

  testResult ( foo_infix (sfull "$4\r\nINFO\r\nTAIL") ) (Done ("INFO", sfull "TAIL")) "foo_infix" ;

  testResult ( choice [(takeStr "PING"), (takeStr "INFO")] (sfull "INFOTAIL") ) (Done ("INFO", sfull "TAIL")) "choice" ;
  ()
)


fun runBench name n f s =
let
  fun runIt f 0 = ()
    | runIt f n = (f s; runIt f (n-1) )

  val t0 = Time.now ()
  val _ = runIt f n
  val t1 = Time.now ()
in
  print (name ^ " " ^ Real.toString(Time.toReal(Time.-(t1, t0))) ^ "\n")
end


val N = 10000000

fun benckmark () = (
    print "Run Benckmark...\n";
    runBench "Benckmark Redis" N foo_infix (sfull "$4\r\nINFO\r\nTAIL");
    runBench "Benckmark CSV  " N scanList  (sfull "4,5\n2,3\n-")
  )


fun main () = (
    sample ();
    benckmark ()
  )
