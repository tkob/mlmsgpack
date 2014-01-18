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
    fn () => doPack packInt 0 = [0],
    fn () => doPack packInt 127 = [127] (* max positive fixnum *),
    fn () => doPack packInt 128 = [0xcc, 128],
    fn () => doPack packInt 0xff = [0xcc, 255] (* max uint 8 *),

    fn () => doPack packInt 0x100 = [0xcd, 0x01, 0x00],
    fn () => doPack packInt 0xffff = [0xcd, 0xff, 0xff] (* max uint 16 *),

    fn () => doPack packInt 0x10000 = [0xce, 0x00, 0x01, 0x00, 0x00],
    fn () => doPack packInt 0x3fffffff = [0xce, 0x3f, 0xff, 0xff, 0xff] (* max int 31 *),
    fn () => Int.precision <? 32 orelse doPack packInt (0x7fff * 0x10000 + 0xffff) = [0xce, 0x7f, 0xff, 0xff, 0xff] (* max int 32 *),
    fn () => Int.precision <? 33 orelse doPack packInt (0xffff * 0x10000 + 0xffff) = [0xce, 0xff, 0xff, 0xff, 0xff] (* max uint 32 *),

    fn () => Int.precision <? 34 orelse doPack packInt (0xffff * 0x10000 + 0xffff + 1) = [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00],
    fn () => Int.precision <? 63 orelse doPack packInt (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 63 *),
    fn () => Int.precision <? 64 orelse doPack packInt (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 64 *),
    fn () => Int.precision <? 65 orelse doPack packInt (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max uint 64 *),

    fn () => doPack packInt ~1 = [0xff],
    fn () => doPack packInt ~32 = [0xe0] (* min negative fixnum *),

    fn () => doPack packInt ~33 = [0xd0, 0xdf],
    fn () => doPack packInt ~128 = [0xd0, 0x80] (* min int 8 *),

    fn () => doPack packInt ~129 = [0xd1, 0xff, 0x7f],
    fn () => doPack packInt ~32768 = [0xd1, 0x80, 0x00] (* min int 16 *),

    fn () => doPack packInt ~32769 = [0xd2, 0xff, 0xff, 0x7f, 0xff],
    fn () => doPack packInt ~1073741824 = [0xd2, 0xc0, 0x00, 0x00, 0x00] (* min int 31 *),
    fn () => Int.precision <? 32 orelse doPack packInt (~0x8000 * 0x10000) (* ~2147483648 *) = [0xd2, 0x80, 0x00, 0x00, 0x00] (* min int 32 *),

    fn () => Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 - 1) (* ~2147483649 *) = [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff],
    fn () => Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) = [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] (* min int 64 *),

    fn () => doPack packUnit () = [0xc0],
    fn () => doPack packBool false = [0xc2],
    fn () => doPack packBool true = [0xc3],

    fn () => doPack (packPair (packInt, packInt)) (1, 2) = [0x92, 1, 2],
    fn () => doPack (packPair (packInt, packInt)) (1, 128) = [0x92, 1, 0xcc, 128],
    fn () => doPack (packPair (packInt, packBool)) (128, true) = [0x92, 0xcc, 128, 0xc3],
    fn () => doPack (packPair (packInt, packPair (packInt, packInt))) (1, (2, 3)) = [0x92, 1, 0x92, 2, 3],
    fn () => doPack (packTuple3 (packInt, packInt, packInt)) (1, 2, 3) = [0x93, 1, 2, 3],
    fn () => doPack (packTuple4 (packInt, packInt, packInt, packInt)) (1, 2, 3, 4) = [0x94, 1, 2, 3, 4],
    fn () => doPack (packTuple5 (packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5) = [0x95, 1, 2, 3, 4, 5],
    fn () => doPack (packTuple6 (packInt, packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5, 6) = [0x96, 1, 2, 3, 4, 5, 6],

    fn () => doPack (packList packInt) [] = [0x90],
    fn () => doPack (packList packInt) [0] = [0x91, 0],
    fn () => doPack (packList packInt) (List.tabulate (15, fn n => n))  = 0x9f::List.tabulate (15, fn n => n),
    fn () => doPack (packList packInt) (List.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1),
    fn () => doPack (packList packInt) (List.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1),

    fn () => doPack (packVector packInt) (Vector.tabulate (0, fn n => n)) = [0x90],
    fn () => doPack (packVector packInt) (Vector.tabulate (1, fn n => n)) =  [0x91, 0],
    fn () => doPack (packVector packInt) (Vector.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n),
    fn () => doPack (packVector packInt) (Vector.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1),
    fn () => doPack (packVector packInt) (Vector.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1),

    fn () => doPack (packArray packInt) (Array.tabulate (0, fn n => n)) = [0x90],
    fn () => doPack (packArray packInt) (Array.tabulate (1, fn n => n)) =  [0x91, 0],
    fn () => doPack (packArray packInt) (Array.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n),
    fn () => doPack (packArray packInt) (Array.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1),
    fn () => doPack (packArray packInt) (Array.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1),

    fn () => true]

  fun doIt () =
    let fun run test =
      if test () then print "." else print "F"
      handle _ => print "E"
    in
      List.app run tests
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
    fn () => doUnpack unpackInt [0] = 0,
    fn () => doUnpack unpackInt [127] = 127(* max positive fixnum *),
    fn () => doUnpack unpackInt [0xcc, 128] = 128,
    fn () => doUnpack unpackInt [0xcc, 255] = 0xff (* max uint 8 *),

    fn () => doUnpack unpackInt [0xcd, 0x01, 0x00] = 0x100 ,
    fn () => doUnpack unpackInt [0xcd, 0xff, 0xff] = 0xffff (* max uint 16 *),

    fn () => doUnpack unpackInt [0xce, 0x00, 0x01, 0x00, 0x00] = 0x10000 ,
    fn () => doUnpack unpackInt [0xce, 0x3f, 0xff, 0xff, 0xff] = 0x3fffffff (* max int 31 *),
    fn () => Int.precision <? 32 orelse doUnpack unpackInt [0xce, 0x7f, 0xff, 0xff, 0xff] = (0x7fff * 0x10000 + 0xffff) (* max int 32 *),
    fn () => Int.precision <? 33 orelse doUnpack unpackInt [0xce, 0xff, 0xff, 0xff, 0xff] = (0xffff * 0x10000 + 0xffff) (* max uint 32 *),

    fn () => Int.precision <? 34 orelse doUnpack unpackInt [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] = (0xffff * 0x10000 + 0xffff + 1),
    fn () => Int.precision <? 63 orelse doUnpack unpackInt [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 63 *),
    fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 64 *),
    fn () => Int.precision <? 65 orelse doUnpack unpackInt [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max uint 64 *),

    fn () => doUnpack unpackInt [0xff] = ~1 ,
    fn () => doUnpack unpackInt [0xe0] = ~32 (* min negative fixnum *),
    fn () => doUnpack unpackInt [0xd0, 0xdf] = ~33 ,
    fn () => doUnpack unpackInt [0xd0, 0x80] = ~128 (* min int 8 *),

    fn () => doUnpack unpackInt [0xd1, 0xff, 0x7f] = ~129 ,
    fn () => doUnpack unpackInt [0xd1, 0x80, 0x00] = ~32768 (* min int 16 *),

    fn () => doUnpack unpackInt [0xd2, 0xff, 0xff, 0x7f, 0xff] = ~32769,
    fn () => doUnpack unpackInt [0xd2, 0xc0, 0x00, 0x00, 0x00] = ~1073741824 (* min int 31 *),
    fn () => Int.precision <? 32 orelse doUnpack unpackInt [0xd2, 0x80, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000) (* ~2147483648 *) (* min int 32 *),

    fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff] = (~0x8000 * 0x10000 - 1) (* ~2147483649 *) ,
    fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) (* min int 64 *),

    fn () => Real.toString (doUnpack unpackReal [0xca, 0xc2, 0xed, 0x40, 0x00]) = "~118.625",
    fn () => Real.toString (doUnpack unpackReal [0xcb, 0xc0, 0x5d, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x00]) = "~118.625",
    fn () => Real.toString (doUnpack unpackReal [0xcb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2e, 0xea]) = "3.14159265359",
    fn () => Real.toString (doUnpack unpackReal [0xcb, 0x40, 0x05, 0xbf, 0x0a, 0x8b, 0x14, 0x5f, 0xcf]) = "2.71828182846",

    fn () => doUnpack unpackUnit [0xc0] = () ,
    fn () => doUnpack unpackBool [0xc2] = false ,
    fn () => doUnpack unpackBool [0xc3] = true ,

    fn () => doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 2] = (1, 2) ,
    fn () => doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 0xcc, 128] = (1, 128) ,
    fn () => doUnpack (unpackPair (unpackInt, unpackBool)) [0x92, 0xcc, 128, 0xc3] = (128, true) ,
    fn () => doUnpack (unpackPair (unpackInt, unpackPair (unpackInt, unpackInt))) [0x92, 1, 0x92, 2, 3] = (1, (2, 3)) ,
    fn () => doUnpack (unpackTuple3 (unpackInt, unpackInt, unpackInt)) [0x93, 1, 2, 3] = (1, 2, 3) ,
    fn () => doUnpack (unpackTuple4 (unpackInt, unpackInt, unpackInt, unpackInt)) [0x94, 1, 2, 3, 4] = (1, 2, 3, 4) ,
    fn () => doUnpack (unpackTuple5 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x95, 1, 2, 3, 4, 5] = (1, 2, 3, 4, 5) ,
    fn () => doUnpack (unpackTuple6 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x96, 1, 2, 3, 4, 5, 6] = (1, 2, 3, 4, 5, 6) ,

    fn () => doUnpack (unpackList unpackInt) [0x90] = [],
    fn () => doUnpack (unpackList unpackInt) [0x91, 0] = [0],
    fn () => doUnpack (unpackList unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (List.tabulate (15, fn n => n)) ,
    fn () => doUnpack (unpackList unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (List.tabulate (65535, fn _ => 1)) ,
    fn () => doUnpack (unpackList unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (List.tabulate (65536, fn _ => 1)) ,

    fn () => doUnpack (unpackVector unpackInt) [0x90] = (Vector.tabulate (0, fn n => n)) ,
    fn () => doUnpack (unpackVector unpackInt) [0x91, 0] = (Vector.tabulate (1, fn n => n)) ,
    fn () => doUnpack (unpackVector unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Vector.tabulate (15, fn n => n)) ,
    fn () => doUnpack (unpackVector unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Vector.tabulate (65535, fn _ => 1)) ,
    fn () => doUnpack (unpackVector unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Vector.tabulate (65536, fn _ => 1)) ,

(*
    fn () => doUnpack (unpackArray unpackInt) [0x90] = (Array.tabulate (0, fn n => n)) ,
    fn () => doUnpack (unpackArray unpackInt) [0x91, 0] = (Array.tabulate (1, fn n => n)) ,
    fn () => doUnpack (unpackArray unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Array.tabulate (15, fn n => n)) ,
    fn () => doUnpack (unpackArray unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Array.tabulate (65535, fn _ => 1)) ,
    fn () => doUnpack (unpackArray unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Array.tabulate (65536, fn _ => 1)) ,
*)
    fn () => true]

  fun doIt () =
    let fun run test =
      if test () then print "." else print "F"
    in
      List.app run tests
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
(*    val realPrecision = Int.toString Real.precision
    val largeRealPrecision = Int.toString LargeReal.precision *)
    fun println s = print (s ^ "\n")
  in
    println ("Int.precision       = " ^ intPrecision);
    println ("LargeInt.precision  = " ^ largeIntPrecision);
    println ("Word.wordSize       = " ^ wordSize);
    println ("LargeWord.wordSize  = " ^ largeWordSize);
(*    println ("Real.precision      = " ^ realPrecision);
    println ("LargeReal.precision = " ^ largeRealPrecision); *)
    PackTest.doIt ();
    UnpackTest.doIt ();
    println "\ndone."
  end
