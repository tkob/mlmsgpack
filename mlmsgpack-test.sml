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

  fun doIt () =
    let
      val true = doPack packInt 0 = [0]
      val true = doPack packInt 127 = [127] (* max positive fixnum *)
      val true = doPack packInt 128 = [0xcc, 128]
      val true = doPack packInt 0xff = [0xcc, 255] (* max uint 8 *)

      val true = doPack packInt 0x100 = [0xcd, 0x01, 0x00]
      val true = doPack packInt 0xffff = [0xcd, 0xff, 0xff] (* max uint 16 *)

      val true = doPack packInt 0x10000 = [0xce, 0x00, 0x01, 0x00, 0x00]
      val true = doPack packInt 0x3fffffff = [0xce, 0x3f, 0xff, 0xff, 0xff] (* max int 31 *)
      val true = Int.precision <? 32 orelse doPack packInt (0x7fff * 0x10000 + 0xffff) = [0xce, 0x7f, 0xff, 0xff, 0xff] (* max int 32 *)
      val true = Int.precision <? 33 orelse doPack packInt (0xffff * 0x10000 + 0xffff) = [0xce, 0xff, 0xff, 0xff, 0xff] (* max uint 32 *)

      val true = Int.precision <? 34 orelse doPack packInt (0xffff * 0x10000 + 0xffff + 1) = [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]
      val true = Int.precision <? 63 orelse doPack packInt (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 63 *)
      val true = Int.precision <? 64 orelse doPack packInt (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 64 *)
      val true = Int.precision <? 65 orelse doPack packInt (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max uint 64 *)

      val true = doPack packInt ~1 = [0xff]
      val true = doPack packInt ~32 = [0xe0] (* min negative fixnum *)

      val true = doPack packInt ~33 = [0xd0, 0xdf]
      val true = doPack packInt ~128 = [0xd0, 0x80] (* min int 8 *)

      val true = doPack packInt ~129 = [0xd1, 0xff, 0x7f]
      val true = doPack packInt ~32768 = [0xd1, 0x80, 0x00] (* min int 16 *)

      val true = doPack packInt ~32769 = [0xd2, 0xff, 0xff, 0x7f, 0xff]
      val true = doPack packInt ~1073741824 = [0xd2, 0xc0, 0x00, 0x00, 0x00] (* min int 31 *)
      val true = Int.precision <? 32 orelse doPack packInt (~0x8000 * 0x10000) (* ~2147483648 *) = [0xd2, 0x80, 0x00, 0x00, 0x00] (* min int 32 *)

      val true = Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 - 1) (* ~2147483649 *) = [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff]
      val true = Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) = [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] (* min int 64 *)

      val true = doPack packUnit () = [0xc0]
      val true = doPack packBool false = [0xc2]
      val true = doPack packBool true = [0xc3]

      val true = doPack (packPair (packInt, packInt)) (1, 2) = [0x92, 1, 2]
      val true = doPack (packPair (packInt, packInt)) (1, 128) = [0x92, 1, 0xcc, 128]
      val true = doPack (packPair (packInt, packBool)) (128, true) = [0x92, 0xcc, 128, 0xc3]
      val true = doPack (packPair (packInt, packPair (packInt, packInt))) (1, (2, 3)) = [0x92, 1, 0x92, 2, 3]
      val true = doPack (packTuple3 (packInt, packInt, packInt)) (1, 2, 3) = [0x93, 1, 2, 3]
      val true = doPack (packTuple4 (packInt, packInt, packInt, packInt)) (1, 2, 3, 4) = [0x94, 1, 2, 3, 4]
      val true = doPack (packTuple5 (packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5) = [0x95, 1, 2, 3, 4, 5]
      val true = doPack (packTuple6 (packInt, packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5, 6) = [0x96, 1, 2, 3, 4, 5, 6]

      val true = doPack (packList packInt) [] = [0x90]
      val true = doPack (packList packInt) [0] = [0x91, 0]
      val true = doPack (packList packInt) (List.tabulate (15, fn n => n))  = 0x9f::List.tabulate (15, fn n => n)
      val true = doPack (packList packInt) (List.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)
      val true = doPack (packList packInt) (List.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)

      val true = doPack (packVector packInt) (Vector.tabulate (0, fn n => n)) = [0x90]
      val true = doPack (packVector packInt) (Vector.tabulate (1, fn n => n)) =  [0x91, 0]
      val true = doPack (packVector packInt) (Vector.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n)
      val true = doPack (packVector packInt) (Vector.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)
      val true = doPack (packVector packInt) (Vector.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)

      val true = doPack (packArray packInt) (Array.tabulate (0, fn n => n)) = [0x90]
      val true = doPack (packArray packInt) (Array.tabulate (1, fn n => n)) =  [0x91, 0]
      val true = doPack (packArray packInt) (Array.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n)
      val true = doPack (packArray packInt) (Array.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)
      val true = doPack (packArray packInt) (Array.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)

    in
      ()
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

  fun doIt () =
    let
      val true = doUnpack unpackInt [0] = 0
      val true = doUnpack unpackInt [127] = 127(* max positive fixnum *)
      val true = doUnpack unpackInt [0xcc, 128] = 128
      val true = doUnpack unpackInt [0xcc, 255] = 0xff (* max uint 8 *)

      val true = doUnpack unpackInt [0xcd, 0x01, 0x00] = 0x100 
      val true = doUnpack unpackInt [0xcd, 0xff, 0xff] = 0xffff (* max uint 16 *)

      val true = doUnpack unpackInt [0xce, 0x00, 0x01, 0x00, 0x00] = 0x10000 
      val true = doUnpack unpackInt [0xce, 0x3f, 0xff, 0xff, 0xff] = 0x3fffffff (* max int 31 *)
      val true = Int.precision <? 32 orelse doUnpack unpackInt [0xce, 0x7f, 0xff, 0xff, 0xff] = (0x7fff * 0x10000 + 0xffff) (* max int 32 *)
      val true = Int.precision <? 33 orelse doUnpack unpackInt [0xce, 0xff, 0xff, 0xff, 0xff] = (0xffff * 0x10000 + 0xffff) (* max uint 32 *)

      val true = Int.precision <? 34 orelse doUnpack unpackInt [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] = (0xffff * 0x10000 + 0xffff + 1)
      val true = Int.precision <? 63 orelse doUnpack unpackInt [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 63 *)
      val true = Int.precision <? 64 orelse doUnpack unpackInt [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 64 *)
      val true = Int.precision <? 65 orelse doUnpack unpackInt [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max uint 64 *)

      val true = doUnpack unpackInt [0xff] = ~1 
      val true = doUnpack unpackInt [0xe0] = ~32 (* min negative fixnum *)
      val true = doUnpack unpackInt [0xd0, 0xdf] = ~33 
      val true = doUnpack unpackInt [0xd0, 0x80] = ~128 (* min int 8 *)

      val true = doUnpack unpackInt [0xd1, 0xff, 0x7f] = ~129 
      val true = doUnpack unpackInt [0xd1, 0x80, 0x00] = ~32768 (* min int 16 *)

      val true = doUnpack unpackInt [0xd2, 0xff, 0xff, 0x7f, 0xff] = ~32769 
      val true = doUnpack unpackInt [0xd2, 0xc0, 0x00, 0x00, 0x00] = ~1073741824 (* min int 31 *)
      val true = Int.precision <? 32 orelse doUnpack unpackInt [0xd2, 0x80, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000) (* ~2147483648 *) (* min int 32 *)

      val true = Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff] = (~0x8000 * 0x10000 - 1) (* ~2147483649 *) 
      val true = Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) (* min int 64 *)

      val true = Real.toString (doUnpack unpackReal [0xca, 0xc2, 0xed, 0x40, 0x00]) = "~118.625"
      val true = Real.toString (doUnpack unpackReal [0xcb, 0xc0, 0x5d, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x00]) = "~118.625"
      val true = Real.toString (doUnpack unpackReal [0xcb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2e, 0xea]) = "3.14159265359"
      val true = Real.toString (doUnpack unpackReal [0xcb, 0x40, 0x05, 0xbf, 0x0a, 0x8b, 0x14, 0x5f, 0xcf]) = "2.71828182846"

      val true = doUnpack unpackUnit [0xc0] = () 
      val true = doUnpack unpackBool [0xc2] = false 
      val true = doUnpack unpackBool [0xc3] = true 

      val true = doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 2] = (1, 2) 
      val true = doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 0xcc, 128] = (1, 128) 
      val true = doUnpack (unpackPair (unpackInt, unpackBool)) [0x92, 0xcc, 128, 0xc3] = (128, true) 
      val true = doUnpack (unpackPair (unpackInt, unpackPair (unpackInt, unpackInt))) [0x92, 1, 0x92, 2, 3] = (1, (2, 3)) 
      val true = doUnpack (unpackTuple3 (unpackInt, unpackInt, unpackInt)) [0x93, 1, 2, 3] = (1, 2, 3) 
      val true = doUnpack (unpackTuple4 (unpackInt, unpackInt, unpackInt, unpackInt)) [0x94, 1, 2, 3, 4] = (1, 2, 3, 4) 
      val true = doUnpack (unpackTuple5 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x95, 1, 2, 3, 4, 5] = (1, 2, 3, 4, 5) 
      val true = doUnpack (unpackTuple6 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x96, 1, 2, 3, 4, 5, 6] = (1, 2, 3, 4, 5, 6) 

      val true = doUnpack (unpackList unpackInt) [0x90] = []
      val true = doUnpack (unpackList unpackInt) [0x91, 0] = [0]
      val true = doUnpack (unpackList unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (List.tabulate (15, fn n => n)) 
      val true = doUnpack (unpackList unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (List.tabulate (65535, fn _ => 1)) 
      val true = doUnpack (unpackList unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (List.tabulate (65536, fn _ => 1)) 

      val true = doUnpack (unpackVector unpackInt) [0x90] = (Vector.tabulate (0, fn n => n)) 
      val true = doUnpack (unpackVector unpackInt) [0x91, 0] = (Vector.tabulate (1, fn n => n)) 
      val true = doUnpack (unpackVector unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Vector.tabulate (15, fn n => n)) 
      val true = doUnpack (unpackVector unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Vector.tabulate (65535, fn _ => 1)) 
      val true = doUnpack (unpackVector unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Vector.tabulate (65536, fn _ => 1)) 

(*
      val true = doUnpack (unpackArray unpackInt) [0x90] = (Array.tabulate (0, fn n => n)) 
      val true = doUnpack (unpackArray unpackInt) [0x91, 0] = (Array.tabulate (1, fn n => n)) 
      val true = doUnpack (unpackArray unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Array.tabulate (15, fn n => n)) 
      val true = doUnpack (unpackArray unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Array.tabulate (65535, fn _ => 1)) 
      val true = doUnpack (unpackArray unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Array.tabulate (65536, fn _ => 1)) 
*)
    in
      ()
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
    println "done."
  end;
