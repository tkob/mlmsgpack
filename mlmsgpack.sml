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
    Int.precision >= 16 andalso Word.wordSize >= 16
*)
val true = case Int.precision of
             SOME p => p >= 16
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
  fun scan bytes =
    let
      val length = Word8Vector.length bytes
      val isMinus = Word8Vector.sub (bytes, 0) >= Word8.fromInt 0x80
      fun loop index mask value =
        let
          val index' = index + 1
          val mask' = W.<< (mask, Word.fromInt 8)
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
      fun loop from to n =
        let
          val n' = n - 1
          val byte = W.andb (from, W.fromInt 0xff)
          val to' = W.orb (to, byte)
        in
          if n' = 0 then to'
          else
            let
              val from' = W.>> (from, Word.fromInt 8)
              val to'' = W.<< (to', Word.fromInt 8)
            in
              loop from' to'' n'
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
            val word' = W.>> (word, Word.fromInt 8)
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

functor MessagePack(structure S : sig
                      type instream
                      type outstream
                      val input1 : instream -> (Word8.word * instream) option
                      val inputN : instream * int -> Word8Vector.vector * instream
                      val output1 : outstream * Word8.word -> unit
                    end) :> sig
  structure Pack : sig
    type 'a packer

    val pack : 'a packer -> 'a -> S.outstream -> unit

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
  end
  structure Unpack : sig
    type 'a unpacker

    val unpack : 'a unpacker -> S.instream -> 'a * S.instream
  
    val || : 'a unpacker * 'a unpacker -> 'a unpacker

    val unpackList : 'a unpacker -> 'a list unpacker

    val unpackPair : 'a unpacker * 'b unpacker -> ('a * 'b) unpacker
    val unpackTuple3 : 'a unpacker * 'b unpacker * 'c unpacker -> ('a * 'b * 'c) unpacker
    val unpackTuple4 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker -> ('a * 'b * 'c * 'd) unpacker
    val unpackTuple5 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker * 'e unpacker -> ('a * 'b * 'c * 'd * 'e) unpacker
    val unpackTuple6 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker * 'e unpacker * 'f unpacker -> ('a * 'b * 'c * 'd * 'e * 'f) unpacker
  
    val unpackUnit : unit unpacker
    val unpackBool : bool unpacker
    val unpackInt : int unpacker
    val unpackLargeInt : LargeInt.int unpacker

    val unpackOption : 'a unpacker -> 'a option unpacker
    val unpackNillable : 'a unpacker -> 'a option unpacker
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

  structure IntMP = struct
    datatype int = Int of Int.int
                 | LargeInt of LargeInt.int
                 | Word8 of Word8.word
                 | Word of Word.word
                 | LargeWord of LargeWord.word
                 | Word8Vector of Word8Vector.vector
                 | Word8VectorSlice of Word8VectorSlice.slice
  end

  fun fixArray length = Word8.orb (Word8.fromInt 0x90, Word8.fromInt length)

  structure Pack = struct
    exception Pack

    type 'a packer = 'a -> S.outstream -> unit

    fun pack p value outs = p value outs

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
            (S.output1 (outs, Word8.fromInt 0xdc);
            UintPrinterIntWord.print length 2 outs)
          else if length div 0x10000 div 0x10000 = 0 then
            (* array 32 *)
            (S.output1 (outs, Word8.fromInt 0xdd);
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

      fun packUnit () outs = S.output1 (outs, Word8.fromInt 0xc0)
      fun packBool bool outs =
        if bool then S.output1 (outs, Word8.fromInt 0xc3)
                else S.output1 (outs, Word8.fromInt 0xc2)
  
      fun packInt int outs = 
        if (int >= 0   andalso int <= 127) orelse
           (int >= ~32 andalso int <=  ~1) then
          S.output1 (outs, Word8.fromInt int)
        else if int > 0 then
          (* positive *)
          if int < 0x100 then
            (* uint 8 *)
            (S.output1 (outs, Word8.fromInt 0xcc);
            S.output1 (outs, Word8.fromInt int))
          else if int < 0x10000 then
            (* uint 16 *)
            (S.output1 (outs, Word8.fromInt 0xcd);
            UintPrinterIntWord.print int 2 outs)
          else if int div 0x10000 div 0x10000 = 0 then
            (* uint 32 *)
            (S.output1 (outs, Word8.fromInt 0xce);
            if Word.wordSize >= 32 then
              UintPrinterIntWord.print int 4 outs
            else if LargeWord.wordSize >= 32 then
              UintPrinterIntLargeWord.print int 4 outs
            else
              (* is there any implementation s.t. LargeWord.wordSize < 32 ? *)
              UintPrinterInfInt.print int 4 outs)
          else if int div 0x10000 div 0x10000 div 0x10000 div 0x10000 = 0 then
            (* uint 64 *)
            (S.output1 (outs, Word8.fromInt 0xcf);
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
            (S.output1 (outs, Word8.fromInt 0xd0);
            S.output1 (outs, Word8.fromInt int))
          else if int >= ~32768 then
            (* int 16 *)
            (S.output1 (outs, Word8.fromInt 0xd1);
            IntPrinterIntWord.print int 2 outs)
          else if int div 0x8000 div 0x10000 = ~1 then
            (* int 32 *)
            (S.output1 (outs, Word8.fromInt 0xd2);
            if Word.wordSize >= 32 then
              IntPrinterIntWord.print int 4 outs
            else if LargeWord.wordSize >= 32 then
              IntPrinterIntLargeWord.print int 4 outs
            else
              IntPrinterInfInt.print int 4 outs)
          else if int div 0x8000 div 0x10000 div 0x10000 div 0x10000 = ~1 then
            (* int 64 *)
            (S.output1 (outs, Word8.fromInt 0xd3);
            if Word.wordSize >= 64 then
              IntPrinterIntWord.print int 8 outs
            else if LargeWord.wordSize >= 64 then
              IntPrinterIntLargeWord.print int 8 outs
            else
              IntPrinterInfInt.print int 8 outs)
          else
            raise Overflow
    end
  end

  structure Unpack = struct
    exception Unpack

    type 'a unpacker = S.instream -> 'a * S.instream

    fun unpack u ins = u ins
  
    infix 0 ||
    fun (u1 || u2) ins = u1 ins handle Unpack => u2 ins

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
      fun unpackArray u length ins =
        let
          fun loop ins n values =
            if n = 0 then (rev values, ins)
            else
              let val (value, ins') = u ins in
                loop ins' (n - 1) (value::values)
              end
        in
          loop ins length []
        end
      fun isFixArray byte = Word8.andb (byte, Word8.fromInt 0xf0) = Word8.fromInt 0x90
      fun lengthOfFixArray byte = Word8.toInt (Word8.andb (byte, Word8.fromInt 0x0f))
      fun unpackFixArray u ins =
        case S.input1 ins of
          SOME (byte, ins')
            => if isFixArray byte then unpackArray u (lengthOfFixArray byte) ins'
               else raise Unpack
        | NONE => raise Unpack
      fun unpackArray16 u ins = 
        let
          val ins' = expect (Word8.fromInt 0xdc) ins
          val (bytes, ins'') = S.inputN (ins', 2)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          unpackArray u length ins
        end
      fun unpackArray32 u ins = 
        let
          val ins' = expect (Word8.fromInt 0xdd) ins
          val (bytes, ins'') = S.inputN (ins', 4)
          val length = UintScannerInt.scan bytes handle Overflow => raise Size
        in
          unpackArray u length ins
        end
    in
      fun unpackList u ins = (
           unpackFixArray u
        || unpackArray16 u
        || unpackArray32 u
      ) ins
    end
 
    fun unpackPair (u1, u2) ins =
      let
        val ins0 = expect (fixArray 2) ins
        val (v1, ins1) = u1 ins
        val (v2, ins2) = u2 ins1
      in
        ((v1, v2), ins2)
      end
  
    fun unpackTuple3 (u1, u2, u3) ins =
      let
        val ins0 = expect (fixArray 3) ins
        val (v1, ins1) = u1 ins
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
      in
        ((v1, v2, v3), ins3)
      end
  
    fun unpackTuple4 (u1, u2, u3, u4) ins =
      let
        val ins0 = expect (fixArray 4) ins
        val (v1, ins1) = u1 ins
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
        val (v4, ins4) = u4 ins3
      in
        ((v1, v2, v3, v4), ins4)
      end
  
    fun unpackTuple5 (u1, u2, u3, u4, u5) ins =
      let
        val ins0 = expect (fixArray 5) ins
        val (v1, ins1) = u1 ins
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
        val (v1, ins1) = u1 ins
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
        byte >= Word8.fromInt 0x00 andalso byte <= Word8.fromInt 0x7f 
      fun isNegativeFixnum byte = 
        byte >= Word8.fromInt 0xe0 andalso byte <= Word8.fromInt 0xff 
      fun isUint8  byte = byte = Word8.fromInt 0xcc
      fun isUint16 byte = byte = Word8.fromInt 0xcd
      fun isUint32 byte = byte = Word8.fromInt 0xce
      fun isUint64 byte = byte = Word8.fromInt 0xcf
      fun isInt8   byte = byte = Word8.fromInt 0xd0
      fun isInt16  byte = byte = Word8.fromInt 0xd1
      fun isInt32  byte = byte = Word8.fromInt 0xd2
      fun isInt64  byte = byte = Word8.fromInt 0xd3

      val fromWord8toWord = Word.fromLargeWord o Word8.toLargeWord
  
      fun unpackFixnum pred f ins =
        case S.input1 ins of
          SOME (byte, ins') => if pred byte then (f byte, ins') else raise Unpack
        | NONE => raise Unpack
      fun unpackPositiveFixnum f ins = unpackFixnum isPositiveFixnum f ins
      fun unpackNegativeFixnum f ins = unpackFixnum isPositiveFixnum f ins
  
      val unpackCategory1 = unpackFixnum
  
      fun unpackCategory2 pred f ins =
        case S.input1 ins of
          SOME (byte, ins') =>
            if pred byte then
              let
                val length = Word8.toInt (Word8.<< (Word8.fromInt 1, fromWord8toWord (Word8.andb (byte, Word8.fromInt 0x03))))
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
    in
      fun unpackUnit ins = unpackCategory1 (fn byte => byte = Word8.fromInt 0xc0) (fn _ => ()) ins
      fun unpackBool ins =
        let
          fun isBool byte = byte = Word8.fromInt 0xc2 orelse byte = Word8.fromInt 0xc3
          fun toBool byte = byte = Word8.fromInt 0xc3
        in
          unpackCategory1 isBool toBool ins
        end
      fun unpackInt ins = (
           unpackPositiveFixnum Word8.toInt
        || unpackNegativeFixnum Word8.toInt
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
    end

    fun unpackOption u ins =
      (u >> SOME || (fn ins => (NONE, ins))) ins

    fun unpackNillable u ins =
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
  fun toList outs = List.rev (!outs)
  fun mkOutstream () : outstream = ref []
end

(* structure MessagePackBinIO = MessagePack(structure S = BinIO.StreamIO)
structure MessagePackBinTextIO = MessagePack(structure S = BinTextIO) *)
structure MessagePackIntListIO = MessagePack(structure S = IntListIO)

structure PackTest = struct
  open MessagePackIntListIO.Pack
  fun doPack p i =
    let val outs = IntListIO.mkOutstream () in
      pack p i outs;
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

