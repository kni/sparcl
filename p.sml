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
        (apR (takeStr "$") (takeInt ()) )
        (fn n =>
            (apR (takeStr "\r\n")
                (apL (takeN n) (takeStr "\r\n"))
            )
        )
    )

val foo_infix =
        takeStr "$" *> takeInt ()
        >>=
        (fn n => takeStr "\r\n" *> takeN n <* takeStr "\r\n")


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
  fun runIt f s 0 = ()
    | runIt f s n = (f s; runIt f s (n-1) )

  val t0 = Time.now ()
  val _ = runIt f s n
  val t1 = Time.now ()
in
  print (name ^ " " ^ Real.toString(Time.toReal(Time.-(t1, t0))) ^ "\n")
end


fun benckmark () = (
    print "Run Benckmark...\n";
    let val s = sfull "$4\r\nINFO\r\nTAIL" in
    runBench "Benckmark Redis" 10000000 foo s
    end
  )


fun main () = (
    sample ();
    benckmark ()
  )
