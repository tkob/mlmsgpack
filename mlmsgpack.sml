signature I = sig
  eqtype int

  val toLarge   : int -> LargeInt.int
  val fromLarge : LargeInt.int -> int
  val fromInt : Int.int -> int
  
  val + : int * int -> int
  val * : int * int -> int
  val div : int * int -> int
end

signature W = sig
  eqtype word
  
  val wordSize : int
  
  val toLarge      : word -> LargeWord.word
  val fromLarge     : LargeWord.word -> word
  val toLargeInt  : word -> LargeInt.int
  val toLargeIntX : word -> LargeInt.int
  val toInt  : word -> int
  val toIntX : word -> int
  val fromInt : int -> word
  
  val andb : word * word -> word
  val orb  : word * word -> word
  val notb : word -> word
  val << : word * Word.word -> word
  val >> : word * Word.word -> word
  
  val + : word * word -> word
  val * : word * word -> word
end

(* 
  requirement:
    Int.precision >= 28 andalso Word.wordSize >= 16
*)
val true = case Int.precision of
             SOME p => p >= 28
           | NONE => true
           andalso Word.wordSize >= 16

(*
  UintScanner scans unsigned integer from Word8Vector and construct I.int value
    I: result integer type
*)
functor UintScanner(I : I) :> sig
  val scan : Word8Vector.vector -> I.int
end = struct
  (* with the expectation that the compiler will optimize this idiom *)
  val fromWord8ToI = I.fromLarge o Word8.toLargeInt
  fun scan bytes =
    let
      fun f (byte, int) = I.+ (I.* (int, I.fromInt 256), fromWord8ToI byte)
    in
      Word8Vector.foldl f (I.fromInt 0) bytes
    end
end

(*
   IntScanner scans signed integer from Word8Vector and construct I.int value
     I: result integer type
     W: word type used for intermediate working memory
     toInt: a function that converts a W value into I
*)
functor IntScanner(structure I : I;
                   structure W : W;
                   val toInt : W.word -> I.int) :> sig
  val scan : Word8Vector.vector -> I.int
end = struct
  (* with the expectation that the compiler will optimize this idiom *)
  val fromWord8ToW = W.fromLarge o Word8.toLarge
  val word8 = Word8.fromLarge o Word.toLarge
  fun scan bytes =
    let
      val length = Word8Vector.length bytes
      val isMinus = Word8Vector.sub (bytes, 0) >= word8 0wx80
      fun loop index mask value =
        let
          val index' = index + 1
          val mask' = W.<< (mask, 0w8)
          val msb = fromWord8ToW (Word8Vector.sub (bytes, index))
          val sum = W.+ (value, msb)
        in
(*          print ("index: " ^ Int.toString index ^ "\n");
          print ("mask: " ^ W.toString mask ^ "\n");
          print ("value: " ^ W.toString value ^ "\n");
          print ("msb: " ^ W.toString msb ^ "\n");
          print ("sum: " ^ W.toString sum ^ "\n");
          print "\n"; *)
          if index' = length then
            if isMinus then toInt (W.orb (sum, mask'))
            else toInt sum
          else loop index' mask' (W.* (sum, W.fromInt 256))
        end
    in
(*      print ("length: " ^ Int.toString length ^ "\n");
      print ("isMinus: " ^ Bool.toString isMinus ^ "\n");
      print "\n"; *)
      loop 0 (W.notb (W.fromInt 0)) (W.fromInt 0)
    end
end

(*
   UintPrinter prints I.int value
     I: integer type
     W: word type used for intermediate working memory
     S: output stream
*)
functor UintPrinter(structure I : I;
                    structure W : W;
                    structure S : sig
                      type outstream
                      val output1 : outstream * Word8.word -> unit
                    end) :> sig
  (* print int n outs *)
  val print : I.int -> int -> S.outstream -> unit
end = struct
  (* with the expectation that the compiler will optimize this idiom *)
  val fromIToW = W.fromLarge o LargeWord.fromLargeInt o I.toLarge
  val fromWToWord8 = Word8.fromLarge o W.toLarge
  fun reverse int n =
    let
      val word = fromIToW int
      fun loop src dest n =
        let
          val n' = n - 1
          val byte = W.andb (src, W.fromInt 0xff)
          val dest' = W.orb (dest, byte)
        in
          if n' = 0 then dest'
          else
            let
              val src' = W.>> (src, 0w8)
              val dest'' = W.<< (dest', 0w8)
            in
              loop src' dest'' n'
            end
        end
    in
      loop word (W.fromInt 0) n
    end
  fun print int n outs =
    let
      val reversed = reverse int n
      fun loop word n =
        if n = 0 then ()
        else
          let
            val byte = fromWToWord8 word
            val word' = W.>> (word, 0w8)
          in
            S.output1 (outs, byte);
            loop word' (n - 1)
          end
    in
      loop reversed n
    end
end

(*
   UintPrinterInf prints I.int value. Word8.word list is used for working memory
     I: integer type
     S: output stream
     fromInt: Word8.fromInt or Word8.fromLargeInt
*)
functor UintPrinterInf(structure I : I;
                       structure S : sig
                         type outstream
                         val output1 : outstream * Word8.word -> unit
                       end;
                       val fromInt : I.int -> Word8.word) :> sig
  val print : I.int -> int -> S.outstream -> unit
end = struct
  fun explode int n =
    let
      fun loop int n bytes =
        if n = 0 then bytes
        else
          let val byte = fromInt int in
            loop (I.div (int, I.fromInt 0x100)) (n - 1) (byte::bytes)
          end
    in
      loop int n []
    end
  fun print int n outs =
    let val exploded = explode int n in
      app (fn byte => S.output1 (outs, byte)) exploded
    end
end

functor BitScanner(structure I : I;
                   val toInt : Word8.word -> I.int) :> sig
  val scan : int -> int -> Word8Vector.vector -> I.int
end = struct
  val fromWordToI = I.fromLarge o Word.toLargeInt
  val word8 = Word8.fromLarge o Word.toLarge
  fun scan src length bytes =
    let
      fun scan src length i dest =
  (*      (print ("src "^Int.toString src^"\n");
        print ("length "^Int.toString length^"\n");
        print ("i "^Int.toString i^"\n");
        print ("dest "^Int.toString dest^"\n");
        print ("=\n"); *)
        if src >= 8 then scan (src - 8) length (i + 1) dest
        else
          let
            val byte = Word8Vector.sub (bytes, i)
            val mask = Word8.>> (word8 0wxff, Word.fromInt src)
            val byte' = Word8.andb (byte, mask) (* omit leading bis *)
          in
           if (length > 8 - src) then
             scan 0 (length - (8 - src)) (i + 1) (I.+ (I.* (dest, I.fromInt 0x100), toInt byte'))
           else
             let val lshift = fromWordToI (Word.<< (0w1, Word.fromInt length)) in
               I.+ (I.* (dest, lshift), toInt (Word8.>> (byte', Word.fromInt (8 - src - length))))
             end
          end
  (* ) *)
    in
      scan src length 0 (I.fromInt 0)
    end
end


functor MessagePack(structure S : sig
                      type instream
                      type outstream
                      val input1 : instream -> (Word8.word * instream) option
                      val inputN : instream * int -> Word8Vector.vector * instream
                      val output : outstream * Word8Vector.vector -> unit
                      val output1 : outstream * Word8.word -> unit
                    end) :> sig
  structure Pack : sig
    type 'a packer

    val doPack : 'a packer -> 'a -> S.outstream -> unit

    val packList   : 'a packer -> 'a list   packer
    val packVector : 'a packer -> 'a vector packer
    val packArray  : 'a packer -> 'a array  packer

    val packPair   : 'a packer * 'b packer -> ('a * 'b) packer
    val packTuple3 : 'a packer * 'b packer * 'c packer -> ('a * 'b * 'c) packer
    val packTuple4 : 'a packer * 'b packer * 'c packer * 'd packer -> ('a * 'b * 'c * 'd) packer
    val packTuple5 : 'a packer * 'b packer * 'c packer * 'd packer * 'e packer -> ('a * 'b * 'c * 'd * 'e) packer
    val packTuple6 : 'a packer * 'b packer * 'c packer * 'd packer * 'e packer * 'f packer -> ('a * 'b * 'c * 'd * 'e * 'f) packer

    val packUnit   : unit packer
    val packBool   : bool packer
    val packInt    : int packer
    val packReal   : real packer
    val packBytes  : Word8Vector.vector packer

    val packOption : 'a packer -> 'a option packer
  end
  structure Unpack : sig
    type 'a unpacker

    val doUnpack : 'a unpacker -> S.instream -> 'a * S.instream
  
    val || : 'a unpacker * 'a unpacker -> 'a unpacker

    val unpackArrayFold : 'a unpacker -> ('a * 'b -> 'b) -> 'b -> 'b unpacker
    val unpackList   : 'a unpacker -> 'a list unpacker
    val unpackVector : 'a unpacker -> 'a vector unpacker
    val unpackArray  : 'a unpacker -> 'a array unpacker

    val unpackPair : 'a unpacker * 'b unpacker -> ('a * 'b) unpacker
    val unpackTuple3 : 'a unpacker * 'b unpacker * 'c unpacker -> ('a * 'b * 'c) unpacker
    val unpackTuple4 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker -> ('a * 'b * 'c * 'd) unpacker
    val unpackTuple5 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker * 'e unpacker -> ('a * 'b * 'c * 'd * 'e) unpacker
    val unpackTuple6 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker * 'e unpacker * 'f unpacker -> ('a * 'b * 'c * 'd * 'e * 'f) unpacker

    val unpackMapFold : ('a unpacker * 'b unpacker)-> ('a * 'b * 'c -> 'c) -> 'c -> 'c unpacker
    val unpackPairList : ('a unpacker * 'b unpacker)-> ('a * 'b) list unpacker
  
    val unpackUnit : unit unpacker
    val unpackBool : bool unpacker
    val unpackInt : int unpacker
    val unpackLargeInt : LargeInt.int unpacker
    val unpackReal : real unpacker
    val unpackBytes : Word8Vector.vector unpacker

    val unpackOption : 'a unpacker -> 'a option unpacker
  end
end = struct
  structure UintScannerInt = UintScanner(Int)
  structure UintScannerLargeInt = UintScanner(LargeInt)
  structure IntScannerIntWord =
    IntScanner(structure I = Int;
               structure W = Word;
               val toInt = W.toIntX)
  structure IntScannerIntLargeWord =
    IntScanner(structure I = Int;
               structure W = LargeWord;
               val toInt = W.toIntX)
  structure IntScannerLargeIntWord =
    IntScanner(structure I = LargeInt;
               structure W = Word;
               val toInt = W.toLargeIntX)
  structure IntScannerLargeIntLargeWord =
    IntScanner(structure I = LargeInt;
               structure W = LargeWord;
               val toInt = W.toLargeIntX)

  structure UintPrinterIntWord =
    UintPrinter(structure I = Int;
                structure W = Word;
                structure S = S)
  structure UintPrinterIntLargeWord =
    UintPrinter(structure I = Int;
                structure W = LargeWord;
                structure S = S)
  structure UintPrinterInfInt =
    UintPrinterInf(structure I = Int;
                   structure S = S;
                   val fromInt = Word8.fromInt)

  structure IntPrinterIntWord = UintPrinterIntWord
  structure IntPrinterIntLargeWord = UintPrinterIntLargeWord
  structure IntPrinterInfInt = UintPrinterInfInt

  structure BitScannerInt = BitScanner(structure I = Int; val toInt = Word8.toInt)
  structure BitScannerLargeInt = BitScanner(structure I = LargeInt; val toInt = Word8.toLargeInt)

  structure IntMP = struct
    datatype int = Int of Int.int
                 | LargeInt of LargeInt.int
                 | Word8 of Word8.word
                 | Word of Word.word
                 | LargeWord of LargeWord.word
                 | Word8Vector of Word8Vector.vector
                 | Word8VectorSlice of Word8VectorSlice.slice
  end

  val word8 = Word8.fromLarge o Word.toLarge
  fun fixArray length = Word8.orb (word8 0wx90, Word8.fromInt length)

  structure Pack = struct
    exception Pack

    type 'a packer = 'a -> S.outstream -> unit

    fun doPack p value outs = p value outs

    local
      (* with the expectation that the compiler will optimize this idiom *)
      val fromWord8ToWord = Word.fromLarge o Word8.toLarge
      val fromWordToWord8 = Word8.fromLarge o Word.toLarge
      fun outputFixArray length outs =
        S.output1 (outs, fixArray length)
      fun packListLike length app p values outs =
        let
          val length = length values
        in
          if length < 0x10 then
            (* FixArray *) 
            outputFixArray length outs
          else if length < 0x10000 then
            (* array 16 *)
            (S.output1 (outs, word8 0wxdc);
            UintPrinterIntWord.print length 2 outs)
          else if length div 0x10000 div 0x10000 = 0 then
            (* array 32 *)
            (S.output1 (outs, word8 0wxdd);
            if Word.wordSize >= 32 then
              UintPrinterIntWord.print length 4 outs
            else if LargeWord.wordSize >= 32 then
              UintPrinterIntLargeWord.print length 4 outs
            else
              UintPrinterInfInt.print length 4 outs)
          else 
            raise Size;
          app (fn value => p value outs) values
        end
    in
      fun packList   p values outs = packListLike List.length   List.app   p values outs
      fun packVector p values outs = packListLike Vector.length Vector.app p values outs
      fun packArray  p values outs = packListLike Array.length  Array.app  p values outs
  
      fun packPair (p1, p2) (v1, v2) outs =
        (outputFixArray 2 outs;
        p1 v1 outs; p2 v2 outs)
      fun packTuple3 (p1, p2, p3) (v1, v2, v3) outs =
        (outputFixArray 3 outs;
        p1 v1 outs; p2 v2 outs; p3 v3 outs)
      fun packTuple4 (p1, p2, p3, p4) (v1, v2, v3, v4) outs =
        (outputFixArray 4 outs;
        p1 v1 outs; p2 v2 outs; p3 v3 outs; p4 v4 outs)
      fun packTuple5 (p1, p2, p3, p4, p5) (v1, v2, v3, v4, v5) outs =
        (outputFixArray 5 outs;
        p1 v1 outs; p2 v2 outs; p3 v3 outs; p4 v4 outs; p5 v5 outs)
      fun packTuple6 (p1, p2, p3, p4, p5, p6) (v1, v2, v3, v4, v5, v6) outs =
        (outputFixArray 6 outs;
        p1 v1 outs; p2 v2 outs; p3 v3 outs; p4 v4 outs; p5 v5 outs; p6 v6 outs)

      fun packUnit () outs = S.output1 (outs, word8 0wxc0)
      fun packBool bool outs =
        if bool then S.output1 (outs, word8 0wxc3)
                else S.output1 (outs, word8 0wxc2)
  
      fun packInt int outs = 
        if (int >= 0   andalso int <= 127) orelse
           (int >= ~32 andalso int <=  ~1) then
          S.output1 (outs, Word8.fromInt int)
        else if int > 0 then
          (* positive *)
          if int < 0x100 then
            (* uint 8 *)
            (S.output1 (outs, word8 0wxcc);
            S.output1 (outs, Word8.fromInt int))
          else if int < 0x10000 then
            (* uint 16 *)
            (S.output1 (outs, word8 0wxcd);
            UintPrinterIntWord.print int 2 outs)
          else if int div 0x10000 div 0x10000 = 0 then
            (* uint 32 *)
            (S.output1 (outs, word8 0wxce);
            if Word.wordSize >= 32 then
              UintPrinterIntWord.print int 4 outs
            else if LargeWord.wordSize >= 32 then
              UintPrinterIntLargeWord.print int 4 outs
            else
              (* is there any implementation s.t. LargeWord.wordSize < 32 ? *)
              UintPrinterInfInt.print int 4 outs)
          else if int div 0x10000 div 0x10000 div 0x10000 div 0x10000 = 0 then
            (* uint 64 *)
            (S.output1 (outs, word8 0wxcf);
            if Word.wordSize >= 64 then
              UintPrinterIntWord.print int 8 outs
            else if LargeWord.wordSize >= 64 then
              UintPrinterIntLargeWord.print int 8 outs
            else
              UintPrinterInfInt.print int 8 outs)
          else
            raise Overflow
        else
          (* negative *)
          if int >= ~128 then
            (* int 8 *)
            (S.output1 (outs, word8 0wxd0);
            S.output1 (outs, Word8.fromInt int))
          else if int >= ~32768 then
            (* int 16 *)
            (S.output1 (outs, word8 0wxd1);
            IntPrinterIntWord.print int 2 outs)
          else if int div 0x8000 div 0x10000 = ~1 then
            (* int 32 *)
            (S.output1 (outs, word8 0wxd2);
            if Word.wordSize >= 32 then
              IntPrinterIntWord.print int 4 outs
            else if LargeWord.wordSize >= 32 then
              IntPrinterIntLargeWord.print int 4 outs
            else
              IntPrinterInfInt.print int 4 outs)
          else if int div 0x8000 div 0x10000 div 0x10000 div 0x10000 = ~1 then
            (* int 64 *)
            (S.output1 (outs, word8 0wxd3);
            if Word.wordSize >= 64 then
              IntPrinterIntWord.print int 8 outs
            else if LargeWord.wordSize >= 64 then
              IntPrinterIntLargeWord.print int 8 outs
            else
              IntPrinterInfInt.print int 8 outs)
          else
            raise Overflow
    end

    fun packReal real outs =
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
        print ("L=" ^ Int.toString siginigicandL' ^ "\n");
        (* print ("sign:"^Int.toString sign^"\n");
        print ("exponent:"^Int.toString exponent^"\n");
        print ("exponent':"^Int.toString exponent'^"\n");
        print ("significandH':"^Int.toString siginigicandH'^"\n"); *)
        S.output1 (outs, word8 0wxcb);
        UintPrinterIntWord.print h 2 outs;
        UintPrinterIntWord.print siginigicandM' 3 outs;
        UintPrinterIntWord.print siginigicandL' 3 outs
      end

    fun packBytes bytes outs =
      let val length = Word8Vector.length bytes in
        if length < 32 then
          (* FixRaw *)
          S.output1 (outs, (Word8.orb (word8 0wxa0, Word8.fromInt length)))
        else if length div 0x10000 = 0 then
          (* raw 16 *)
          (S.output1 (outs, word8 0wxda);
          UintPrinterIntWord.print length 2 outs)
        else if length div 0x10000 div 0x10000 = 0 then
          (* raw 32 *)
          (S.output1 (outs, word8 0wxdb);
          if Word.wordSize >= 32 then
            UintPrinterIntWord.print length 4 outs
          else if LargeWord.wordSize >= 32 then
            UintPrinterIntLargeWord.print length 4 outs
          else
            UintPrinterInfInt.print length 4 outs)
        else
          raise Size;
        S.output (outs, bytes)
      end

    fun packOption p option outs =
      case option of 
        SOME value => p value outs
      | NONE => packUnit () outs

  end

  structure Unpack = struct
    exception Unpack

    type 'a unpacker = S.instream -> 'a * S.instream

    fun doUnpack u ins = u ins
  
    (* alternate *)
    infix 0 ||
    fun (u1 || u2) ins = u1 ins handle Unpack => u2 ins

    (* concatenate *)
    infix 5 --
    fun (u1 -- u2) ins = 
      let val (v1, ins1) = u1 ins
          val (v2, ins2) = u2 ins1
      in
        ((v1, v2), ins2)
      end

    (* transformation *)
    infix 3 >>
    fun (u >> f) ins =
      let val (v, ins') = u ins in
        (f v, ins')
      end

    fun expect expected ins =
      case S.input1 ins of
        SOME (byte, ins') => if byte = expected then ins' else raise Unpack
      | NONE => raise Unpack

    local
      fun isFixArray byte = Word8.andb (byte, word8 0wxf0) = word8 0wx90
      fun isFixMap byte = Word8.andb (byte, word8 0wxf0) = word8 0wx80
      fun lengthOfFix byte = Word8.toInt (Word8.andb (byte, word8 0wx0f))
      fun scanFixArray ins =
        case S.input1 ins of
          SOME (byte, ins')
            => if isFixArray byte then (lengthOfFix byte, ins')
               else raise Unpack
        | NONE => raise Unpack
      fun scanArray16 ins = 
        let
          val ins' = expect (word8 0wxdc) ins
          val (bytes, ins'') = S.inputN (ins', 2)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          (length, ins'')
        end
      fun scanArray32 ins = 
        let
          val ins' = expect (word8 0wxdd) ins
          val (bytes, ins'') = S.inputN (ins', 4)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          (length, ins'')
        end
      fun scanFixMap ins =
        case S.input1 ins of
          SOME (byte, ins')
            => if isFixMap byte then (lengthOfFix byte, ins')
               else raise Unpack
        | NONE => raise Unpack
      fun scanMap16 ins = 
        let
          val ins' = expect (word8 0wxde) ins
          val (bytes, ins'') = S.inputN (ins', 2)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          (length, ins'')
        end
      fun scanMap32 ins = 
        let
          val ins' = expect (word8 0wxdf) ins
          val (bytes, ins'') = S.inputN (ins', 4)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          (length, ins'')
        end
      fun unpackFold length u f init ins =
        let
          fun loop ins n acc =
            if n = 0 then (acc, ins)
            else
              let val (value, ins') = u ins in
                loop ins' (n - 1) (f (value, acc))
              end
        in
          loop ins length init
        end
    in
      fun unpackArrayFold u f init ins =
        let
          val (length, ins') = (scanFixArray || scanArray16 || scanArray32) ins
        in
          unpackFold length u f init ins'
        end

      fun unpackList u ins = (unpackArrayFold u (op::) [] >> rev) ins 
      fun unpackVector u ins = (unpackList u >> Vector.fromList) ins
      fun unpackArray u ins = (unpackList u >> Array.fromList) ins

      fun unpackMapFold (u1, u2) f init ins =
        let
          val (length, ins') = (scanFixMap || scanMap16 || scanMap32) ins
        in
          unpackFold length (u1 -- u2) (fn ((a, b), c) => f (a, b, c)) init ins'
        end

      fun unpackPairList (u1, u2) ins =
        unpackMapFold (u1, u2) (fn (k, v, l) => (k, v)::l) [] ins
    end
 
    fun unpackPair (u1, u2) ins =
      let
        val ins0 = expect (fixArray 2) ins
        val (v1, ins1) = u1 ins0
        val (v2, ins2) = u2 ins1
      in
        ((v1, v2), ins2)
      end
  
    fun unpackTuple3 (u1, u2, u3) ins =
      let
        val ins0 = expect (fixArray 3) ins
        val (v1, ins1) = u1 ins0
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
      in
        ((v1, v2, v3), ins3)
      end
  
    fun unpackTuple4 (u1, u2, u3, u4) ins =
      let
        val ins0 = expect (fixArray 4) ins
        val (v1, ins1) = u1 ins0
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
        val (v4, ins4) = u4 ins3
      in
        ((v1, v2, v3, v4), ins4)
      end
  
    fun unpackTuple5 (u1, u2, u3, u4, u5) ins =
      let
        val ins0 = expect (fixArray 5) ins
        val (v1, ins1) = u1 ins0
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
        val (v4, ins4) = u4 ins3
        val (v5, ins5) = u5 ins4
      in
        ((v1, v2, v3, v4, v5), ins5)
      end
  
    fun unpackTuple6 (u1, u2, u3, u4, u5, u6) ins =
      let
        val ins0 = expect (fixArray 6) ins
        val (v1, ins1) = u1 ins0
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
        val (v4, ins4) = u4 ins3
        val (v5, ins5) = u5 ins4
        val (v6, ins6) = u6 ins5
      in
        ((v1, v2, v3, v4, v5, v6), ins6)
      end
  
    local 
      fun isPositiveFixnum byte =
        byte >= (word8 0wx00) andalso byte <= (word8 0wx7f)
      fun isNegativeFixnum byte =
        byte >= (word8 0wxe0) andalso byte <= (word8 0wxff)
      fun isUint8  byte = byte = word8 0wxcc
      fun isUint16 byte = byte = word8 0wxcd
      fun isUint32 byte = byte = word8 0wxce
      fun isUint64 byte = byte = word8 0wxcf
      fun isInt8   byte = byte = word8 0wxd0
      fun isInt16  byte = byte = word8 0wxd1
      fun isInt32  byte = byte = word8 0wxd2
      fun isInt64  byte = byte = word8 0wxd3
      fun isFloat  byte = byte = word8 0wxca
      fun isDouble byte = byte = word8 0wxcb

      val fromWord8toWord = Word.fromLarge o Word8.toLarge
  
      fun unpackFixnum pred f ins =
        case S.input1 ins of
          SOME (byte, ins') => if pred byte then (f byte, ins') else raise Unpack
        | NONE => raise Unpack
      fun unpackPositiveFixnum f ins = unpackFixnum isPositiveFixnum f ins
      fun unpackNegativeFixnum f ins = unpackFixnum isNegativeFixnum f ins
  
      val unpackCategory1 = unpackFixnum
 
      fun unpackCategory2 pred f ins =
        case S.input1 ins of
          SOME (byte, ins') =>
            if pred byte then
              let
                val length = Word8.toInt (Word8.<< (word8 0w1, fromWord8toWord (Word8.andb (byte, word8 0wx03))))
                val (bytes, ins'') = S.inputN (ins', length)
              in
                (f bytes, ins'')
              end
            else raise Unpack
        | NONE => raise Unpack
      fun unpackUint8  f ins = unpackCategory2 isUint8  f ins
      fun unpackUint16 f ins = unpackCategory2 isUint16 f ins
      fun unpackUint32 f ins = unpackCategory2 isUint32 f ins
      fun unpackUint64 f ins = unpackCategory2 isUint64 f ins
      fun unpackInt8   f ins = unpackCategory2 isInt8   f ins
      fun unpackInt16  f ins = unpackCategory2 isInt16  f ins
      fun unpackInt32  f ins = unpackCategory2 isInt32  f ins
      fun unpackInt64  f ins = unpackCategory2 isInt64  f ins
     
      fun scanFloat bytes =
        let
          val sign = if BitScannerInt.scan 0 1 bytes = 0 then 1.0 else ~1.0
          val exponent = Real.fromInt (BitScannerInt.scan 1 8 bytes - 127)
          val extraBit = 0x800000 (* 1 << 23 *)
          val significand = Real.fromInt (BitScannerInt.scan 9 23 bytes + extraBit) * Math.pow(2.0, ~23.0)
        in
          (* print ("sign="^Real.toString sign^"\n");
          print ("exponent="^Real.toString exponent^"\n");
          print ("significand="^Real.toString significand^"\n"); *)
          sign * significand * Math.pow(2.0, exponent)
        end
      fun scanDouble bytes =
        let
          val sign = if BitScannerInt.scan 0 1 bytes = 0 then 1.0 else ~1.0
          val exponent = Real.fromInt (BitScannerInt.scan 1 11 bytes - 1023)
          val extraBit = 0x4000000 (* 1 << 26 *)
          val significandH = Real.fromInt (BitScannerInt.scan 12 26 bytes + extraBit) * Math.pow (2.0, ~26.0)
          val significandL = Real.fromInt (BitScannerInt.scan 38 26 bytes) * Math.pow (2.0, ~52.0)
          val significand = significandH + significandL
        in
          sign * significand * Math.pow(2.0, exponent)
        end

      fun unpackFloat  ins = unpackCategory2 isFloat  scanFloat  ins
      fun unpackDouble ins = unpackCategory2 isDouble scanDouble ins
    in
      fun unpackUnit ins = unpackCategory1 (fn byte => byte = word8 0wxc0) (fn _ => ()) ins
      fun unpackBool ins =
        let
          fun isBool byte = byte = word8 0wxc2 orelse byte = word8 0wxc3
          fun toBool byte = byte = word8 0wxc3
        in
          unpackCategory1 isBool toBool ins
        end
      fun unpackInt ins = (
           unpackPositiveFixnum Word8.toInt
        || unpackNegativeFixnum Word8.toIntX
        || unpackUint8  (fn bytes => Word8.toInt  (Word8Vector.sub (bytes, 0)))
        || unpackInt8   (fn bytes => Word8.toIntX (Word8Vector.sub (bytes, 0)))
        || unpackUint16 UintScannerInt.scan
        || unpackInt16   IntScannerIntWord.scan
        || unpackUint32 UintScannerInt.scan
        || unpackInt32   IntScannerIntLargeWord.scan
        || unpackUint64 UintScannerInt.scan
        || unpackInt64   IntScannerIntLargeWord.scan
      ) ins
      fun unpackLargeInt ins = (
           unpackPositiveFixnum Word8.toLargeInt 
        || unpackNegativeFixnum Word8.toLargeIntX
        || unpackUint8 (fn bytes => Word8.toLargeInt  (Word8Vector.sub (bytes, 0)))
        || unpackInt8  (fn bytes => Word8.toLargeIntX (Word8Vector.sub (bytes, 0)))
        || unpackUint16 UintScannerLargeInt.scan
        || unpackInt16   IntScannerLargeIntWord.scan
        || unpackUint32 UintScannerLargeInt.scan
        || unpackInt32   IntScannerLargeIntLargeWord.scan
        || unpackUint64 UintScannerLargeInt.scan
        || unpackInt64   IntScannerLargeIntLargeWord.scan
      ) ins

      fun unpackReal ins = (
           unpackDouble
        || unpackFloat
      ) ins
    end

    local
      fun unpackRaw length ins = S.inputN (ins, length)
      fun isFixRaw byte = Word8.andb (byte, word8 0wxe0) = word8 0wxa0
      fun lengthOfFixRaw byte = Word8.toInt (Word8.andb (byte, word8 0wx1f))
      fun unpackFixRaw ins =
        case S.input1 ins of
          SOME (byte, ins')
            => if isFixRaw byte then unpackRaw (lengthOfFixRaw byte) ins'
               else raise Unpack
        | NONE => raise Unpack
      fun unpackRaw16 ins = 
        let
          val ins' = expect (word8 0wxda) ins
          val (bytes, ins'') = S.inputN (ins', 2)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          unpackRaw length ins''
        end
      fun unpackRaw32 ins = 
        let
          val ins' = expect (word8 0wxdb) ins
          val (bytes, ins'') = S.inputN (ins', 4)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          unpackRaw length ins''
        end
    in
      fun unpackBytes ins = (
           unpackFixRaw
        || unpackRaw16
        || unpackRaw32
      ) ins
    end

    fun unpackOption u ins =
      (u >> SOME || unpackUnit >> (fn () => NONE)) ins
  
  end
end

(*
structure BinTextIO :> sig
  type instream
  type outstream
  val stdIn : instream
  val stdOut : outstream
  val input1 : instream -> (Word8.word * instream) option
  val inputN : instream * int -> Word8Vector.vector * instream
  val output1 : outstream * Word8.word -> unit
end = struct
  type instream = TextIO.StreamIO.instream 
  type outstream = TextIO.outstream 
  val stdIn = TextIO.getInstream TextIO.stdIn
  val stdOut = TextIO.stdOut
  fun input1 ins =
    case TextIO.StreamIO.input1 ins of
      SOME (char, ins') => SOME (Byte.charToByte char, ins')
    | NONE => NONE
  fun inputN (ins, n) =
    let val (string, ins') = TextIO.StreamIO.inputN (ins, n) in
      (Byte.stringToBytes string, ins')
    end
  fun output1 (outs, byte) = TextIO.output1 (outs, (Byte.byteToChar byte))
end
*)

structure IntListIO = struct
  type instream = int list
  type outstream = int list ref
  fun input1 [] = NONE
    | input1 (x::xs) = SOME (Word8.fromInt x, xs)
  fun inputN (ins, n) =
    let
      val vector = Word8Vector.fromList (map Word8.fromInt (List.take (ins, n)))
      val ins' = List.drop (ins, n)
    in
      (vector, ins')
    end
  fun output1 (outs, byte) =
    outs := (Word8.toInt byte)::(!outs)
  fun output (outs, bytes) =
    Word8Vector.app (fn byte => output1 (outs, byte)) bytes
  fun toList outs = List.rev (!outs)
  fun mkOutstream () : outstream = ref []
end

(* structure MessagePackBinIO = MessagePack(structure S = BinIO.StreamIO)
structure MessagePackBinTextIO = MessagePack(structure S = BinTextIO) *)
structure MessagePackIntListIO = MessagePack(structure S = IntListIO)

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

(*
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

val _ = main ()
*)
