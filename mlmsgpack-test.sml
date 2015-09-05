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

  val minPos = 0.5 * Math.pow (2.0, ~1000.0) * Math.pow (2.0, ~73.0)
  val minNormalPos = 0.5 * Math.pow (2.0, ~1000.0) * Math.pow(2.0, ~21.0)
  fun maxFinite () =  (9007199254740991.0 * (Math.pow (2.0, 971.0)))
  val maxSubnormal = (9007199254740991.0 * (Math.pow (2.0, ~1074.0)) - minNormalPos)

  val tests = [
    ("packInt1", fn () => doPack packInt 0 = [0]),
    ("packInt2", fn () => doPack packInt 127 = [127] (* max positive fixnum *)),
    ("packInt3", fn () => doPack packInt 128 = [0xcc, 128]),
    ("packInt4", fn () => doPack packInt 0xff = [0xcc, 255] (* max uint 8 *)),

    ("packInt5", fn () => doPack packInt 0x100 = [0xcd, 0x01, 0x00]),
    ("packInt6", fn () => doPack packInt 0xffff = [0xcd, 0xff, 0xff] (* max uint 16 *)),

    ("packInt7", fn () => doPack packInt 0x10000 = [0xce, 0x00, 0x01, 0x00, 0x00]),
    ("packInt8", fn () => doPack packInt 0x3fffffff = [0xce, 0x3f, 0xff, 0xff, 0xff] (* max int 31 *)),
    ("packInt9", fn () => Int.precision <? 32 orelse doPack packInt (0x7fff * 0x10000 + 0xffff) = [0xce, 0x7f, 0xff, 0xff, 0xff] (* max int 32 *)),
    ("packInt10", fn () => Int.precision <? 33 orelse doPack packInt (0xffff * 0x10000 + 0xffff) = [0xce, 0xff, 0xff, 0xff, 0xff] (* max uint 32 *)),

    ("packInt11", fn () => Int.precision <? 34 orelse doPack packInt (0xffff * 0x10000 + 0xffff + 1) = [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00]),
    ("packInt12", fn () => Int.precision <? 63 orelse doPack packInt (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 63 *)),
    ("packInt13", fn () => Int.precision <? 64 orelse doPack packInt (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max int 64 *)),
    ("packInt14", fn () => Int.precision <? 65 orelse doPack packInt (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) = [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] (* max uint 64 *)),

    ("packInt15", fn () => doPack packInt ~1 = [0xff]),
    ("packInt16", fn () => doPack packInt ~32 = [0xe0] (* min negative fixnum *)),

    ("packInt17", fn () => doPack packInt ~33 = [0xd0, 0xdf]),
    ("packInt18", fn () => doPack packInt ~128 = [0xd0, 0x80] (* min int 8 *)),

    ("packInt19", fn () => doPack packInt ~129 = [0xd1, 0xff, 0x7f]),
    ("packInt20", fn () => doPack packInt ~32768 = [0xd1, 0x80, 0x00] (* min int 16 *)),

    ("packInt21", fn () => doPack packInt ~32769 = [0xd2, 0xff, 0xff, 0x7f, 0xff]),
    ("packInt22", fn () => doPack packInt ~1073741824 = [0xd2, 0xc0, 0x00, 0x00, 0x00] (* min int 31 *)),
    ("packInt23", fn () => Int.precision <? 32 orelse doPack packInt (~0x8000 * 0x10000) (* ~2147483648 *) = [0xd2, 0x80, 0x00, 0x00, 0x00] (* min int 32 *)),

    ("packInt24", fn () => Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 - 1) (* ~2147483649 *) = [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff]),
    ("packInt25", fn () => Int.precision <? 64 orelse doPack packInt (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) = [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] (* min int 64 *)),

    ("packReal1", fn () => doPack packReal 0.0 = [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal2", fn () => doPack packReal ~0.0 = [0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal3", fn () => doPack packReal (0.0 / 0.0) = [0xcb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
                    orelse doPack packReal (0.0 / 0.0) = [0xcb, 0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal4", fn () => doPack packReal (~0.0 / 0.0) = [0xcb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
                    orelse doPack packReal (0.0 / 0.0) = [0xcb, 0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal5", fn () => doPack packReal (1.0 / 0.0) = [0xcb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal6", fn () => doPack packReal (~1.0 / 0.0) = [0xcb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal7", fn () => doPack packReal Math.pi = [0xcb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18]),
    ("packReal8", fn () => doPack packReal (Math.pi * ~1.0) = [0xcb, 0xc0, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18]),
    ("packReal9", fn () => doPack packReal (maxFinite ())= [0xcb, 0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
    ("packReal10", fn () => doPack packReal (maxFinite () * ~1.0) = [0xcb, 0xff, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
    ("packReal11", fn () => doPack packReal minPos = [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]),
    ("packReal12", fn () => doPack packReal (minPos * ~1.0) = [0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]),
    ("packReal13", fn () => doPack packReal minNormalPos = [0xcb, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal14", fn () => doPack packReal (minNormalPos * ~1.0) = [0xcb, 0x80, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    ("packReal15", fn () => doPack packReal maxSubnormal = [0xcb, 0x00, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
    ("packReal16", fn () => doPack packReal (maxSubnormal * ~1.0) = [0xcb, 0x80, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),

    ("packUnit1", fn () => doPack packUnit () = [0xc0]),
    ("packBool1", fn () => doPack packBool false = [0xc2]),
    ("packBool2", fn () => doPack packBool true = [0xc3]),

    ("packPair1", fn () => doPack (packPair (packInt, packInt)) (1, 2) = [0x92, 1, 2]),
    ("packPair2", fn () => doPack (packPair (packInt, packInt)) (1, 128) = [0x92, 1, 0xcc, 128]),
    ("packPair3", fn () => doPack (packPair (packInt, packBool)) (128, true) = [0x92, 0xcc, 128, 0xc3]),
    ("packPair4", fn () => doPack (packPair (packInt, packPair (packInt, packInt))) (1, (2, 3)) = [0x92, 1, 0x92, 2, 3]),
    ("packTuple1", fn () => doPack (packTuple3 (packInt, packInt, packInt)) (1, 2, 3) = [0x93, 1, 2, 3]),
    ("packTuple2", fn () => doPack (packTuple4 (packInt, packInt, packInt, packInt)) (1, 2, 3, 4) = [0x94, 1, 2, 3, 4]),
    ("packTuple3", fn () => doPack (packTuple5 (packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5) = [0x95, 1, 2, 3, 4, 5]),
    ("packTuple4", fn () => doPack (packTuple6 (packInt, packInt, packInt, packInt, packInt, packInt)) (1, 2, 3, 4, 5, 6) = [0x96, 1, 2, 3, 4, 5, 6]),

    ("packList1", fn () => doPack (packList packInt) [] = [0x90]),
    ("packList2", fn () => doPack (packList packInt) [0] = [0x91, 0]),
    ("packList3", fn () => doPack (packList packInt) (List.tabulate (15, fn n => n))  = 0x9f::List.tabulate (15, fn n => n)),
    ("packList4", fn () => doPack (packList packInt) (List.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)),
    ("packList5", fn () => doPack (packList packInt) (List.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)),

    ("packVector1", fn () => doPack (packVector packInt) (Vector.tabulate (0, fn n => n)) = [0x90]),
    ("packVector2", fn () => doPack (packVector packInt) (Vector.tabulate (1, fn n => n)) =  [0x91, 0]),
    ("packVector3", fn () => doPack (packVector packInt) (Vector.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n)),
    ("packVector4", fn () => doPack (packVector packInt) (Vector.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)),
    ("packVector5", fn () => doPack (packVector packInt) (Vector.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)),

    ("packArray1", fn () => doPack (packArray packInt) (Array.tabulate (0, fn n => n)) = [0x90]),
    ("packArray2", fn () => doPack (packArray packInt) (Array.tabulate (1, fn n => n)) =  [0x91, 0]),
    ("packArray3", fn () => doPack (packArray packInt) (Array.tabulate (15, fn n => n)) = 0x9f::List.tabulate (15, fn n => n)),
    ("packArray4", fn () => doPack (packArray packInt) (Array.tabulate (65535, fn _ => 1)) = [0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)),
    ("packArray5", fn () => doPack (packArray packInt) (Array.tabulate (65536, fn _ => 1)) = [0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)),

    ("packExt1", fn () => doPack (packExtOfType 0) (Word8Vector.tabulate (1, fn n => 0w1)) = [0xd4, 0, 1]),
    ("packExt2", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (1, fn n => 0w1)) = [0xd4, 127, 1]),
    ("packExt3", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (2, fn n => 0w1)) = [0xd5, 127, 1, 1]),
    ("packExt4", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (4, fn n => 0w1)) = [0xd6, 127, 1, 1, 1, 1]),
    ("packExt5", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (8, fn n => 0w1)) = [0xd7, 127, 1, 1, 1, 1, 1, 1, 1, 1]),
    ("packExt6", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (16, fn n => 0w1)) = [0xd8, 127] @ List.tabulate (16, fn n => 1)),
    ("packExt7", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (3, fn n => 0w1)) = [0xc7, 3, 127] @ List.tabulate (3, fn n => 1)),
    ("packExt8", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (5, fn n => 0w1)) = [0xc7, 5, 127] @ List.tabulate (5, fn n => 1)),
    ("packExt9", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (7, fn n => 0w1)) = [0xc7, 7, 127] @ List.tabulate (7, fn n => 1)),
    ("packExt10", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (9, fn n => 0w1)) = [0xc7, 9, 127] @ List.tabulate (9, fn n => 1)),
    ("packExt11", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (15, fn n => 0w1)) = [0xc7, 15, 127] @ List.tabulate (15, fn n => 1)),
    ("packExt12", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (17, fn n => 0w1)) = [0xc7, 17, 127] @ List.tabulate (17, fn n => 1)),
    ("packExt13", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (255, fn n => 0w1)) = [0xc7, 255, 127] @ List.tabulate (255, fn n => 1)),
    ("packExt14", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (0, fn n => 0w1)) = [0xc7, 0, 127]),
    ("packExt15", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (256, fn n => 0w1)) = [0xc8, 0x01, 0x00, 127] @ List.tabulate (256, fn n => 1)),
    ("packExt16", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (65535, fn n => 0w1)) = [0xc8, 0xff, 0xff, 127] @ List.tabulate (65535, fn n => 1)),
    ("packExt17", fn () => doPack (packExtOfType 127) (Word8Vector.tabulate (65536, fn n => 0w1)) = [0xc9, 0x00, 0x01, 0x00, 0x00, 127] @ List.tabulate (65536, fn n => 1)),

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

  fun powOf2 e = Math.pow (2.0, e)

  val minPos = powOf2 (~1022.0 - 52.0)
  val minNormalPos = powOf2 ~1022.0
  fun maxFinite () = (2.0 - powOf2 ~52.0) * powOf2 1023.0
  val maxSubnormal = 9007199254740991.0(* 0x1FFFFFFFFFFFFF *) * powOf2 (~1022.0 - 52.0) - minNormalPos

  val minPos32 = powOf2 (~126.0 - 23.0)
  val minNormalPos32 = powOf2 ~126.0
  val maxFinite32 = (2.0 - powOf2 ~23.0) * powOf2 127.0
  val maxSubnormal32 = Real.fromInt 0xffffff * powOf2 (~126.0 - 23.0) - minNormalPos32

  val tests = [
    ("unpackInt1", fn () => doUnpack unpackInt [0] = 0),
    ("unpackInt2", fn () => doUnpack unpackInt [127] = 127(* max positive fixnum *)),
    ("unpackInt3", fn () => doUnpack unpackInt [0xcc, 128] = 128),
    ("unpackInt4", fn () => doUnpack unpackInt [0xcc, 255] = 0xff (* max uint 8 *)),

    ("unpackInt5", fn () => doUnpack unpackInt [0xcd, 0x01, 0x00] = 0x100 ),
    ("unpackInt6", fn () => doUnpack unpackInt [0xcd, 0xff, 0xff] = 0xffff (* max uint 16 *)),

    ("unpackInt7", fn () => doUnpack unpackInt [0xce, 0x00, 0x01, 0x00, 0x00] = 0x10000 ),
    ("unpackInt8", fn () => doUnpack unpackInt [0xce, 0x3f, 0xff, 0xff, 0xff] = 0x3fffffff (* max int 31 *)),
    ("unpackInt9", fn () => Int.precision <? 32 orelse doUnpack unpackInt [0xce, 0x7f, 0xff, 0xff, 0xff] = (0x7fff * 0x10000 + 0xffff) (* max int 32 *)),
    ("unpackInt10", fn () => Int.precision <? 33 orelse doUnpack unpackInt [0xce, 0xff, 0xff, 0xff, 0xff] = (0xffff * 0x10000 + 0xffff) (* max uint 32 *)),

    ("unpackInt11", fn () => Int.precision <? 34 orelse doUnpack unpackInt [0xcf, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00] = (0xffff * 0x10000 + 0xffff + 1)),
    ("unpackInt12", fn () => Int.precision <? 63 orelse doUnpack unpackInt [0xcf, 0x3f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x3fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 63 *)),
    ("unpackInt13", fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xcf, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0x7fff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max int 64 *)),
    ("unpackInt14", fn () => Int.precision <? 65 orelse doUnpack unpackInt [0xcf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] = (((0xffff * 0x10000 + 0xffff) * 0x10000 + 0xffff) * 0x10000 + 0xffff) (* max uint 64 *)),

    ("unpackInt15", fn () => doUnpack unpackInt [0xff] = ~1 ),
    ("unpackInt16", fn () => doUnpack unpackInt [0xe0] = ~32 (* min negative fixnum *)),
    ("unpackInt17", fn () => doUnpack unpackInt [0xd0, 0xdf] = ~33 ),
    ("unpackInt18", fn () => doUnpack unpackInt [0xd0, 0x80] = ~128 (* min int 8 *)),

    ("unpackInt19", fn () => doUnpack unpackInt [0xd1, 0xff, 0x7f] = ~129 ),
    ("unpackInt20", fn () => doUnpack unpackInt [0xd1, 0x80, 0x00] = ~32768 (* min int 16 *)),

    ("unpackInt21", fn () => doUnpack unpackInt [0xd2, 0xff, 0xff, 0x7f, 0xff] = ~32769),
    ("unpackInt22", fn () => doUnpack unpackInt [0xd2, 0xc0, 0x00, 0x00, 0x00] = ~1073741824 (* min int 31 *)),
    ("unpackInt23", fn () => Int.precision <? 32 orelse doUnpack unpackInt [0xd2, 0x80, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000) (* ~2147483648 *) (* min int 32 *)),

    ("unpackInt24", fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff] = (~0x8000 * 0x10000 - 1) (* ~2147483649 *) ),
    ("unpackInt25", fn () => Int.precision <? 64 orelse doUnpack unpackInt [0xd3, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00] = (~0x8000 * 0x10000 * 0x10000 * 0x10000) (* ~9223372036854775808 *) (* min int 64 *)),

    ("unpackReal1", fn () => Real.== (doUnpack unpackReal [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], 0.0)),
    ("unpackReal2", fn () => Real.== (doUnpack unpackReal [0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ~0.0)),
    ("unpackReal3", fn () => Real.isNan (doUnpack unpackReal [0xcb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
    ("unpackReal4", fn () => Real.isNan (doUnpack unpackReal [0xcb, 0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])),
    ("unpackReal5", fn () => Real.== (doUnpack unpackReal [0xcb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], 1.0 / 0.0)),
    ("unpackReal6", fn () => Real.== (doUnpack unpackReal [0xcb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], ~1.0 / 0.0)),
    ("unpackReal7", fn () => Real.== (doUnpack unpackReal [0xcb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18], Math.pi)),
    ("unpackReal8", fn () => Real.== (doUnpack unpackReal [0xcb, 0xc0, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18], Math.pi * ~1.0)),
    ("unpackReal9", fn () => Real.== (doUnpack unpackReal [0xcb, 0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], maxFinite ())),
    ("unpackReal10", fn () => Real.== (doUnpack unpackReal [0xcb, 0xff, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], maxFinite ()  * ~1.0)),
    ("unpackReal11", fn () => Real.== (doUnpack unpackReal [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], minPos)),
    ("unpackReal12", fn () => Real.== (doUnpack unpackReal [0xcb, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], minPos * ~1.0)),
    ("unpackReal13", fn () => Real.== (doUnpack unpackReal [0xcb, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], minNormalPos)),
    ("unpackReal14", fn () => Real.== (doUnpack unpackReal [0xcb, 0x80, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], minNormalPos * ~1.0)),
    ("unpackReal15", fn () => Real.== (doUnpack unpackReal [0xcb, 0x00, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], maxSubnormal)),
    ("unpackReal16", fn () => Real.== (doUnpack unpackReal [0xcb, 0x80, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], maxSubnormal * ~1.0)),

    ("unpackReal1'", fn () => Real.== (doUnpack unpackReal [0xca, 0x00, 0x00, 0x00, 0x00], 0.0)),
    ("unpackReal2'", fn () => Real.== (doUnpack unpackReal [0xca, 0x80, 0x00, 0x00, 0x00], ~0.0)),
    ("unpackReal3'", fn () => Real.isNan (doUnpack unpackReal [0xca, 0x7f, 0xc0, 0x00, 0x00])),
    ("unpackReal4'", fn () => Real.isNan (doUnpack unpackReal [0xca, 0xff, 0xc0, 0x00, 0x00])),
    ("unpackReal5'", fn () => Real.== (doUnpack unpackReal [0xca, 0x7f, 0x80, 0x00, 0x00], 1.0 / 0.0)),
    ("unpackReal6'", fn () => Real.== (doUnpack unpackReal [0xca, 0xff, 0x80, 0x00, 0x00], ~1.0 / 0.0)),
    ("unpackReal9'", fn () => Real.== (doUnpack unpackReal [0xca, 0x7f, 0x7f, 0xff, 0xff], maxFinite32)),
    ("unpackReal10'", fn () => Real.== (doUnpack unpackReal [0xca, 0xff, 0x7f, 0xff, 0xff], maxFinite32 * ~1.0)),
    ("unpackReal11'", fn () => Real.== (doUnpack unpackReal [0xca, 0x00, 0x00, 0x00, 0x01], minPos32)),
    ("unpackReal12'", fn () => Real.== (doUnpack unpackReal [0xca, 0x80, 0x00, 0x00, 0x01], minPos32 * ~1.0)),
    ("unpackReal13'", fn () => Real.== (doUnpack unpackReal [0xca, 0x00, 0x80, 0x00, 0x00], minNormalPos32)),
    ("unpackReal14'", fn () => Real.== (doUnpack unpackReal [0xca, 0x80, 0x80, 0x00, 0x00], minNormalPos32 * ~1.0)),
    ("unpackReal15'", fn () => Real.== (doUnpack unpackReal [0xca, 0x00, 0x7f, 0xff, 0xff], maxSubnormal32)),
    ("unpackReal16'", fn () => Real.== (doUnpack unpackReal [0xca, 0x80, 0x7f, 0xff, 0xff], maxSubnormal32 * ~1.0)),

    ("unpackUnit1", fn () => doUnpack unpackUnit [0xc0] = () ),
    ("unpackBool1", fn () => doUnpack unpackBool [0xc2] = false ),
    ("unpackBool2", fn () => doUnpack unpackBool [0xc3] = true ),

    ("unpackPair1", fn () => doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 2] = (1, 2) ),
    ("unpackPair2", fn () => doUnpack (unpackPair (unpackInt, unpackInt)) [0x92, 1, 0xcc, 128] = (1, 128) ),
    ("unpackPair3", fn () => doUnpack (unpackPair (unpackInt, unpackBool)) [0x92, 0xcc, 128, 0xc3] = (128, true) ),
    ("unpackPair4", fn () => doUnpack (unpackPair (unpackInt, unpackPair (unpackInt, unpackInt))) [0x92, 1, 0x92, 2, 3] = (1, (2, 3)) ),

    ("unpackTuple1", fn () => doUnpack (unpackTuple3 (unpackInt, unpackInt, unpackInt)) [0x93, 1, 2, 3] = (1, 2, 3) ),
    ("unpackTuple2", fn () => doUnpack (unpackTuple4 (unpackInt, unpackInt, unpackInt, unpackInt)) [0x94, 1, 2, 3, 4] = (1, 2, 3, 4) ),
    ("unpackTuple3", fn () => doUnpack (unpackTuple5 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x95, 1, 2, 3, 4, 5] = (1, 2, 3, 4, 5) ),
    ("unpackTuple4", fn () => doUnpack (unpackTuple6 (unpackInt, unpackInt, unpackInt, unpackInt, unpackInt, unpackInt)) [0x96, 1, 2, 3, 4, 5, 6] = (1, 2, 3, 4, 5, 6) ),

    ("unpackList1", fn () => doUnpack (unpackList unpackInt) [0x90] = []),
    ("unpackList2", fn () => doUnpack (unpackList unpackInt) [0x91, 0] = [0]),
    ("unpackList3", fn () => doUnpack (unpackList unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (List.tabulate (15, fn n => n)) ),
    ("unpackList4", fn () => doUnpack (unpackList unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (List.tabulate (65535, fn _ => 1)) ),
    ("unpackList5", fn () => doUnpack (unpackList unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (List.tabulate (65536, fn _ => 1)) ),

    ("unpackVector1", fn () => doUnpack (unpackVector unpackInt) [0x90] = (Vector.tabulate (0, fn n => n)) ),
    ("unpackVector2", fn () => doUnpack (unpackVector unpackInt) [0x91, 0] = (Vector.tabulate (1, fn n => n)) ),
    ("unpackVector3", fn () => doUnpack (unpackVector unpackInt) (0x9f::List.tabulate (15, fn n => n)) = (Vector.tabulate (15, fn n => n)) ),
    ("unpackVector4", fn () => doUnpack (unpackVector unpackInt) ([0xdc, 0xff, 0xff] @ List.tabulate (65535, fn _ => 1)) = (Vector.tabulate (65535, fn _ => 1)) ),
    ("unpackVector5", fn () => doUnpack (unpackVector unpackInt) ([0xdd, 0x00, 0x01, 0x00, 0x00] @ List.tabulate (65536, fn _ => 1)) = (Vector.tabulate (65536, fn _ => 1)) ),

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
