structure PackTest = struct
  open MessagePackIntListIO.Pack
  fun doPack p value =
    let val outs = IntListIO.mkOutstream () in
      MessagePackIntListIO.Pack.doPack p value outs;
      IntListIO.toList outs
    end
  
  infix 4 <?
  fun (precision <? n) =
    case precision of
      NONE => false
    | SOME p => p < n

  val tests = [
    ("1", fn () => doPack packInt 0 = [0]),
    ("2", fn () => doPack packInt 127 = [127] (* max positive fixnum *)),
    ("3", fn () => doPack packInt 128 = [0xcc, 128]),
    ("4", fn () => doPack packInt 0xff = [0xcc, 255] (* max uint 8 *)),

    ("5", fn () => doPack packInt 0x100 = [0xcd, 0x01, 0x00]),
    ("6", fn () => doPack packInt 0xffff = [0xcd, 0xff, 0xff] (* max uint 16 *)),

    ("7", fn () => doPack packInt 0x10000 = [0xce, 0x00, 0x01, 0x00, 0x00]),
    ("8", fn () => doPack packInt 0x3fffffff = [0xce, 0x3f, 0xff, 0xff, 0xff] (* max int 31 *)),
    ("9", fn () => Int.precision <? 32 orelse doPack packInt (0x7fff * 0x10000 + 0xffff) = [0xce, 0x7f, 0xff, 0xff, 0xff] (* max int 32 *)),
    ("10", fn () => Int.precision <? 33 orelse doPack packInt (0xffff * 0x10000 + 0xffff) = [0xce, 0xff, 0xff, 0xff, 0xff] (* max uint 32 *)),

    ("11", fn () => Int.precision <? 34 orelse doPack packInt (0xffff * 0x10000 + 0xffff + 1) = [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]),
    ("12", fn () => Int.precision <? 63 orelse doPack packInt (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 63 *)),
    ("13", fn () => Int.precision <? 64 orelse doPack packInt (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 64 *)),
    ("14", fn () => Int.precision <? 65 orelse doPack packInt (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max uint 64 *)),

    ("15", fn () => doPack packInt ~1 = [0xff]),
    ("16", fn () => doPack packInt ~32 = [0xe0] (* min negative fixnum *)),

    ("17", fn () => doPack packInt ~33 = [0xd0, 0xdf]),
    ("18", fn () => doPack packInt ~128 = [0xd0, 0x80] (* min int 8 *)),

    ("19", fn () => doPack packInt ~129 = [0xd1, 0xff, 0x7f]),
    ("20", fn () => doPack packInt ~32768 = [0xd1, 0x80, 0x00] (* min int 16 *)),

    ("21", fn () => doPack packInt ~32769 = [0xd2, 0xff, 0xff, 0x7f, 0xff]),
    ("22", fn () => doPack packInt ~1073741824 = [0xd2, 0xc0, 0x00, 0x00, 0x00] (* min int 31 *)),
    ("23", fn () => Int.precision <? 32 orelse doPack packInt (~0x8000 * 0x10000) (* ~2147483648 *) = [0xd2, 0x80, 0x00, 0x00, 0x00] (* min int 32 *)),

    ("24", fn () => Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 - 1) (* ~2147483649 *) = [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff]),
    ("25", fn () => Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) = [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] (* min int 64 *)),


    ("26", fn () => doPack packReal 0.0 = [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("27", fn () => doPack packReal ~0.0 = [0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("28", fn () => doPack packReal (0.0 / 0.0) = [0xcb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("29", fn () => doPack packReal (~0.0 / 0.0) = [0xcb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("30", fn () => doPack packReal Real.posInf = [0xcb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("31", fn () => doPack packReal Real.negInf = [0xcb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("32", fn () => doPack packReal Math.pi = [0xcb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18]),
    ("33", fn () => doPack packReal (Math.pi * ~1.0) = [0xcb, 0xc0, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18]),
    ("34", fn () => doPack packReal Real.maxFinite = [0xcb, 0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
    ("35", fn () => doPack packReal (Real.maxFinite * ~1.0) = [0xcb, 0xff, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
    ("36", fn () => doPack packReal Real.minPos = [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]),
    ("37", fn () => doPack packReal (Real.minPos * ~1.0) = [0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]),
    ("38", fn () => doPack packReal Real.minNormalPos = [0xcb, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("39", fn () => doPack packReal (Real.minNormalPos * ~1.0) = [0xcb, 0x80, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
(*
    fun () => doPack packReal (PackRealBig.fromBytes (Word8Vector.fromList [0wx00, 0wx0F, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF]));
    fun () => doPack packReal (PackRealBig.fromBytes (Word8Vector.fromList [0wx80, 0wx0F, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF, 0wxFF]));
*)

    ("40", fn () => doPack packUnit () = [0xc0]),
    ("41", fn () => doPack packBool false = [0xc2]),
    ("42", fn () => doPack packBool true = [0xc3]),

    ("43", fn () => doPack (packPair (packInt, packInt)) (1, 2) = [0x92, 1, 2]),
    ("44", fn () => doPack (packPair (packInt, packInt)) (1, 128) = [0x92, 1, 0xcc, 128]),
    ("45", fn () => doPack (packPair (packInt, packBool)) (128, true) = [0x92, 0xcc, 128, 0xc3]),
    ("46", fn () => doPack (packPair (packInt, packPair (packInt, packInt))) (1, (2, 3)) = [0x92, 1, 0x92, 2, 3]),
    ("47", fn () => doPack (packTuple3 (packInt, packInt, packInt)) (1, 2, 3) = [0x93, 1, 2, 3]),
    ("48", fn () => doPack (packTuple4 (packInt, packInt, packInt, packInt)) (1, 2, 3, 4) = [0x94, 1, 2, 3, 4]),
    ("49", fn () => doPack (packTuple5 (packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5) = [0x95, 1, 2, 3, 4, 5]),
    ("50", fn () => doPack (packTuple6 (packInt, packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5, 6) = [0x96, 1, 2, 3, 4, 5, 6]),

    ("51", fn () => doPack (packList packInt) [] = [0x90]),
    ("52", fn () => doPack (packList packInt) [0] = [0x91, 0]),
    ("53", fn () => doPack (packList packInt) (List.tabulate (15, fn n => n))  = 0x9f::List.tabulate (15, fn n => n)),
    ("54", fn () => doPack (packList packInt) (List.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)),
    ("55", fn () => doPack (packList packInt) (List.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)),

    ("56", fn () => doPack (packVector packInt) (Vector.tabulate (0, fn n => n)) = [0x90]),
    ("57", fn () => doPack (packVector packInt) (Vector.tabulate (1, fn n => n)) =  [0x91, 0]),
    ("58", fn () => doPack (packVector packInt) (Vector.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n)),
    ("59", fn () => doPack (packVector packInt) (Vector.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)),
    ("60", fn () => doPack (packVector packInt) (Vector.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)),

    ("61", fn () => doPack (packArray packInt) (Array.tabulate (0, fn n => n)) = [0x90]),
    ("62", fn () => doPack (packArray packInt) (Array.tabulate (1, fn n => n)) =  [0x91, 0]),
    ("63", fn () => doPack (packArray packInt) (Array.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n)),
    ("64", fn () => doPack (packArray packInt) (Array.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)),
    ("65", fn () => doPack (packArray packInt) (Array.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)),

    ("_", fn () => true)]

  datatype fail = Fail of string | Error of string * exn

  fun doIt () =
    let
      fun printResult r = case r of
        Fail label => print (label ^ " failed.\n")
      | Error (label, exn) => print (label ^ " failed: " ^ exnMessage exn ^ "\n")
      fun run tests failed =
        case tests of
          [] => (print "\n"; List.app (fn r => printResult r) (rev failed))
        | (label, test):: tests =>
          (if test () then (print "."; run tests failed)
          else (print "F"; run tests (Fail label::failed)))
          handle exn => (print "E"; run tests (Error (label, exn)::failed))
    in
      run tests []
    end
end

structure UnpackTest = struct
  open MessagePackIntListIO.Unpack

  fun doUnpack u ins =
    let val (value, _) = MessagePackIntListIO.Unpack.doUnpack u ins in
      value
    end

  infix 4 <?
  fun (precision <? n) =
    case precision of
      NONE => false
    | SOME p => p < n

  val tests = [
    ("1", fn () => doUnpack unpackInt [0] = 0),
    ("2", fn () => doUnpack unpackInt [127] = 127(* max positive fixnum *)),
    ("3", fn () => doUnpack unpackInt [0xcc, 128] = 128),
    ("4", fn () => doUnpack unpackInt [0xcc, 255] = 0xff (* max uint 8 *)),

    ("5", fn () => doUnpack unpackInt [0xcd, 0x01, 0x00] = 0x100 ),
    ("6", fn () => doUnpack unpackInt [0xcd, 0xff, 0xff] = 0xffff (* max uint 16 *)),

    ("7", fn () => doUnpack unpackInt [0xce, 0x00, 0x01, 0x00, 0x00] = 0x10000 ),
    ("8", fn () => doUnpack unpackInt [0xce, 0x3f, 0xff, 0xff, 0xff] = 0x3fffffff (* max int 31 *)),
    ("9", fn () => Int.precision <? 32 orelse doUnpack unpackInt [0xce, 0x7f, 0xff, 0xff, 0xff] = (0x7fff * 0x10000 + 0xffff) (* max int 32 *)),
    ("10", fn () => Int.precision <? 33 orelse doUnpack unpackInt [0xce, 0xff, 0xff, 0xff, 0xff] = (0xffff * 0x10000 + 0xffff) (* max uint 32 *)),

    ("11", fn () => Int.precision <? 34 orelse doUnpack unpackInt [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] = (0xffff * 0x10000 + 0xffff + 1)),
    ("12", fn () => Int.precision <? 63 orelse doUnpack unpackInt [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 63 *)),
    ("13", fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 64 *)),
    ("14", fn () => Int.precision <? 65 orelse doUnpack unpackInt [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max uint 64 *)),

    ("15", fn () => doUnpack unpackInt [0xff] = ~1 ),
    ("16", fn () => doUnpack unpackInt [0xe0] = ~32 (* min negative fixnum *)),
    ("17", fn () => doUnpack unpackInt [0xd0, 0xdf] = ~33 ),
    ("18", fn () => doUnpack unpackInt [0xd0, 0x80] = ~128 (* min int 8 *)),

    ("19", fn () => doUnpack unpackInt [0xd1, 0xff, 0x7f] = ~129 ),
    ("20", fn () => doUnpack unpackInt [0xd1, 0x80, 0x00] = ~32768 (* min int 16 *)),

    ("21", fn () => doUnpack unpackInt [0xd2, 0xff, 0xff, 0x7f, 0xff] = ~32769),
    ("22", fn () => doUnpack unpackInt [0xd2, 0xc0, 0x00, 0x00, 0x00] = ~1073741824 (* min int 31 *)),
    ("23", fn () => Int.precision <? 32 orelse doUnpack unpackInt [0xd2, 0x80, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000) (* ~2147483648 *) (* min int 32 *)),

    ("24", fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff] = (~0x8000 * 0x10000 - 1) (* ~2147483649 *) ),
    ("25", fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) (* min int 64 *)),

    ("26", fn () => Real.toString (doUnpack unpackReal [0xca, 0xc2, 0xed, 0x40, 0x00]) = "~118.625"),
    ("27", fn () => Real.toString (doUnpack unpackReal [0xcb, 0xc0, 0x5d, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x00]) = "~118.625"),
    ("28", fn () => Real.toString (doUnpack unpackReal [0xcb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2e, 0xea]) = "3.14159265359"),
    ("29", fn () => Real.toString (doUnpack unpackReal [0xcb, 0x40, 0x05, 0xbf, 0x0a, 0x8b, 0x14, 0x5f, 0xcf]) = "2.71828182846"),

    ("30", fn () => doUnpack unpackUnit [0xc0] = () ),
    ("31", fn () => doUnpack unpackBool [0xc2] = false ),
    ("32", fn () => doUnpack unpackBool [0xc3] = true ),

    ("33", fn () => doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 2] = (1, 2) ),
    ("34", fn () => doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 0xcc, 128] = (1, 128) ),
    ("35", fn () => doUnpack (unpackPair (unpackInt, unpackBool)) [0x92, 0xcc, 128, 0xc3] = (128, true) ),
    ("36", fn () => doUnpack (unpackPair (unpackInt, unpackPair (unpackInt, unpackInt))) [0x92, 1, 0x92, 2, 3] = (1, (2, 3)) ),
    ("37", fn () => doUnpack (unpackTuple3 (unpackInt, unpackInt, unpackInt)) [0x93, 1, 2, 3] = (1, 2, 3) ),
    ("38", fn () => doUnpack (unpackTuple4 (unpackInt, unpackInt, unpackInt, unpackInt)) [0x94, 1, 2, 3, 4] = (1, 2, 3, 4) ),
    ("39", fn () => doUnpack (unpackTuple5 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x95, 1, 2, 3, 4, 5] = (1, 2, 3, 4, 5) ),
    ("40", fn () => doUnpack (unpackTuple6 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x96, 1, 2, 3, 4, 5, 6] = (1, 2, 3, 4, 5, 6) ),

    ("41", fn () => doUnpack (unpackList unpackInt) [0x90] = []),
    ("42", fn () => doUnpack (unpackList unpackInt) [0x91, 0] = [0]),
    ("43", fn () => doUnpack (unpackList unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (List.tabulate (15, fn n => n)) ),
    ("44", fn () => doUnpack (unpackList unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (List.tabulate (65535, fn _ => 1)) ),
    ("45", fn () => doUnpack (unpackList unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (List.tabulate (65536, fn _ => 1)) ),

    ("46", fn () => doUnpack (unpackVector unpackInt) [0x90] = (Vector.tabulate (0, fn n => n)) ),
    ("47", fn () => doUnpack (unpackVector unpackInt) [0x91, 0] = (Vector.tabulate (1, fn n => n)) ),
    ("48", fn () => doUnpack (unpackVector unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Vector.tabulate (15, fn n => n)) ),
    ("49", fn () => doUnpack (unpackVector unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Vector.tabulate (65535, fn _ => 1)) ),
    ("50", fn () => doUnpack (unpackVector unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Vector.tabulate (65536, fn _ => 1)) ),

(*
    fn () => doUnpack (unpackArray unpackInt) [0x90] = (Array.tabulate (0, fn n => n)) ,
    fn () => doUnpack (unpackArray unpackInt) [0x91, 0] = (Array.tabulate (1, fn n => n)) ,
    fn () => doUnpack (unpackArray unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Array.tabulate (15, fn n => n)) ,
    fn () => doUnpack (unpackArray unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Array.tabulate (65535, fn _ => 1)) ,
    fn () => doUnpack (unpackArray unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Array.tabulate (65536, fn _ => 1)) ,
*)
    ("_", fn () => true)]

  datatype fail = Fail of string | Error of string * exn

  fun doIt () =
    let
      fun printResult r = case r of
        Fail label => print (label ^ " failed.\n")
      | Error (label, exn) => print (label ^ " failed: " ^ exnMessage exn ^ "\n")
      fun run tests failed =
        case tests of
          [] => (print "\n"; List.app (fn r => printResult r) (rev failed))
        | (label, test):: tests =>
          (if test () then (print "."; run tests failed)
          else (print "F"; run tests (Fail label::failed)))
          handle exn => (print "E"; run tests (Error (label, exn)::failed))
    in
      run tests []
    end
end

fun main () = 
  let
    fun showPrecision (SOME precision) = Int.toString precision
      | showPrecision NONE = "infinite"
    val intPrecision = showPrecision Int.precision
    val largeIntPrecision = showPrecision LargeInt.precision
    val wordSize = Int.toString Word.wordSize
    val largeWordSize = Int.toString LargeWord.wordSize
    fun println s = print (s ^ "\n")
  in
    println ("Int.precision       = " ^ intPrecision);
    println ("LargeInt.precision  = " ^ largeIntPrecision);
    println ("Word.wordSize       = " ^ wordSize);
    println ("LargeWord.wordSize  = " ^ largeWordSize);
    PackTest.doIt ();
    UnpackTest.doIt ();
    println "done."
  end

val _ = main ()
