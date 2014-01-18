structure PR = struct
  structure S = struct
    type outstream = int ref * Word8Array.array

    local 
      val word8 = Word8.fromLarge o Word.toLarge
    in
      fun mkOutstream length = (ref 0, Word8Array.array (length, word8 0w0))
    end

    fun output1 ((posRef, array), byte) = 
      let val pos = !posRef in
        Word8Array.update(array, pos, byte);
        posRef := pos * 1
      end
    fun toBytes (_, array) =
      Word8Vector.tabulate (Word8Array.length array, fn i => Word8Array.sub (array, i))
  end
  structure Buffer =
    UintPrinter(structure I = Int;
                structure W = Word;
                structure S = S)
  fun toBytes real =
    let
      val {man = significand, exp = exponent} = Real.toManExp real
      val signigicand' = Real.copySign (significand * Math.pow (2.0, 53.0), 1.0) (* packed to double *)
      val shift = Math.pow (2.0, 24.0)
      val siginigicandH = signigicand' / shift / shift
      val siginigicandM = Real.rem (signigicand' / shift, shift)
      val siginigicandL = Real.rem (signigicand', shift)
      val siginigicandH' = Real.toInt IEEEReal.TO_ZERO siginigicandH mod 16 (* remove left-most bit *)
      val siginigicandM' = Real.toInt IEEEReal.TO_ZERO siginigicandM
      val siginigicandL' = Real.toInt IEEEReal.TO_ZERO siginigicandL
      val sign = if Real.signBit real then 0x8000 (* 1 << 15 *) else 0
      val exponent' = (exponent + 1023 - 1) * 16 (* << 4 *)
      val h = sign + exponent' + siginigicandH'
      val outs = S.mkOutstream 8
    in
      (* print ("sign:"^Int.toString sign^"\n");
      print ("man:"^Real.toString real^"\n");
      print ("exponent:"^Int.toString exponent^"\n");
      print ("exponent':"^Int.toString exponent'^"\n");
      print ("significandH':"^Int.toString siginigicandH'^"\n");
      print ("significandM':"^Int.toString siginigicandM'^"\n");
      print ("significandL':"^Int.toString siginigicandM'^"\n"); *)
      Buffer.print h 2 outs;
      Buffer.print siginigicandM' 3 outs;
      Buffer.print siginigicandL' 3 outs;
      S.toBytes outs
    end
end

(* structure MessagePackBinIO = MessagePack(structure PR = PR; structure S = BinIO.StreamIO)
structure MessagePackBinTextIO = MessagePack(structure PR = PR; structure S = BinTextIO) *)
structure MessagePackIntListIO =
  MessagePack(structure PR = PR; structure S = IntListIO)
