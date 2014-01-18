functor RealPrinter(structure S : sig
                      type outstream
                      val output1 : outstream * Word8.word -> unit
                    end) :> sig
  (* print int n outs *)
  val print : real -> S.outstream -> unit
end = struct
  structure UintPrinterIntWord =
    UintPrinter(structure I = Int;
                structure W = Word;
                structure S = S)
  fun print real outs =
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
    in
      (* print ("sign:"^Int.toString sign^"\n");
      print ("man:"^Real.toString real^"\n");
      print ("exponent:"^Int.toString exponent^"\n");
      print ("exponent':"^Int.toString exponent'^"\n");
      print ("significandH':"^Int.toString siginigicandH'^"\n");
      print ("significandM':"^Int.toString siginigicandM'^"\n");
      print ("significandL':"^Int.toString siginigicandM'^"\n"); *)
      UintPrinterIntWord.print h 2 outs;
      UintPrinterIntWord.print siginigicandM' 3 outs;
      UintPrinterIntWord.print siginigicandL' 3 outs
    end
end
