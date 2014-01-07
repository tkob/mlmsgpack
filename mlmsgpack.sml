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
functor UintScanner(I : INTEGER) :> sig
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
functor IntScanner(structure I : INTEGER;
                   structure W : WORD;
                   val toInt : W.word -> I.int) :> sig
  val scan : Word8Vector.vector -> I.int
end = struct
  (* with the expectation that the compiler will optimize this idiom *)
  val fromWord8ToW = W.fromLargeWord o Word8.toLargeWord
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
functor UintPrinter(structure I : INTEGER;
                    structure W : WORD;
                    structure S : sig
                      type outstream
                      val output1 : outstream * Word8.word -> unit
                    end) :> sig
  (* print int n outs: int must be positive *)
  val print : I.int -> int -> S.outstream -> unit
end = struct
  (* with the expectation that the compiler will optimize this idiom *)
  val fromIToW = W.fromLargeWord o LargeWord.fromLargeInt o I.toLarge
  val fromWToWord8 = Word8.fromLargeWord o W.toLargeWord
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
*)
functor UintPrinterInf(structure I : INTEGER;
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
        if n = 0 then []
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

    val packPair   : 'a packer * 'b packer -> ('a * 'b) packer
    val packList   : 'a packer -> 'a list   packer
    val packVector : 'a packer -> 'a vector packer
    val packArray  : 'a packer -> 'a array  packer
    val packUnit   : unit packer
    val packBool   : bool packer
    val packInt    : int packer
  end
  structure Unpack : sig
    type 'a unpacker

    val unpack : 'a unpacker -> S.instream -> 'a * S.instream
  
    val || : 'a unpacker * 'a unpacker -> 'a unpacker
    val unpackOption : 'a unpacker -> 'a option unpacker
    val unpackPair : 'a unpacker * 'b unpacker -> ('a * 'b) unpacker
    val unpackTuple3 : 'a unpacker * 'b unpacker * 'c unpacker -> ('a * 'b * 'c) unpacker
    val unpackTuple4 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker -> ('a * 'b * 'c * 'd) unpacker
    val unpackTuple5 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker * 'e unpacker -> ('a * 'b * 'c * 'd * 'e) unpacker
    val unpackTuple6 : 'a unpacker * 'b unpacker * 'c unpacker * 'd unpacker * 'e unpacker * 'f unpacker -> ('a * 'b * 'c * 'd * 'e * 'f) unpacker
  
    val unpackUnit : unit unpacker
    val unpackBool : bool unpacker
    val unpackInt : int unpacker
    val unpackLargeInt : LargeInt.int unpacker
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

  structure IntMP = struct
    datatype int = Int of Int.int
                 | LargeInt of LargeInt.int
                 | Word8 of Word8.word
                 | Word of Word.word
                 | LargeWord of LargeWord.word
                 | Word8Vector of Word8Vector.vector
                 | Word8VectorSlice of Word8VectorSlice.slice
  end

  structure Pack = struct
    exception Pack

    type 'a packer = 'a -> S.outstream -> unit

    fun pack p value outs = p value outs

    local
      (* with the expectation that the compiler will optimize this idiom *)
      val fromWord8ToWord = Word.fromLargeWord o Word8.toLargeWord
      val fromWordToWord8 = Word8.fromLargeWord o Word.toLargeWord
      fun packLength int n outs =
        let
          fun loop word n bytes =
            if n = 0 then
              List.app (fn byte => S.output1 (outs, byte)) bytes
            else
              let
                val byte = fromWordToWord8 (Word.andb (word, Word.fromInt 0xff))
                val word' = Word.>> (word, Word.fromInt 8)
              in
                loop word' (n - 1) (byte::bytes)
              end
        in
          loop (Word.fromInt int) n []
        end
      fun packListLike length app p values outs =
        let
          val length = length values
        in
          if length < 0x10 then
            (* FixArray *) 
            S.output1 (outs, Word8.orb (Word8.fromInt 0x90, Word8.fromInt length))
          else if length < 0x10000 then
            (* array 16 *)
            (S.output1 (outs, Word8.fromInt 0xdc); packLength length 2 outs)
          else
            (* array 32 *)
            (S.output1 (outs, Word8.fromInt 0xdd); packLength length 4 outs);
          app (fn value => p value outs) values
        end
    in
      fun packPair (p1, p2) (v1, v2) outs = (p1 v1 outs; p2 v2 outs)
      fun packList   p values outs = packListLike List.length   List.app   p values outs
      fun packVector p values outs = packListLike Vector.length Vector.app p values outs
      fun packArray  p values outs = packListLike Array.length  Array.app  p values outs
  
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
          if int < 0x10000 then
            (* uint 16 *)
            (S.output1 (outs, Word8.fromInt 0xcd);
            UintPrinterIntWord.print int 2 outs)
          else if int div 0x10000 div 0x10000= 0 then
            (* uint 32 *)
            (S.output1 (outs, Word8.fromInt 0xce);
            if Word.wordSize >= 32 then
              UintPrinterIntWord.print int 4 outs
            else if LargeWord.wordSize >= 32 then
              UintPrinterIntLargeWord.print int 4 outs
            else
              (* is there any implementation s.t. LargeWord.wordSize < 32 ? *)
              UintPrinterInfInt.print int 8 outs)
          else
            (* uint 64 *)
            (S.output1 (outs, Word8.fromInt 0xcf);
            if Word.wordSize >= 64 then
              UintPrinterIntWord.print int 8 outs
            else if LargeWord.wordSize >= 64 then
              UintPrinterIntLargeWord.print int 8 outs
            else
              UintPrinterInfInt.print int 8 outs)
        else
          (* negative *)
          raise Pack (* todo *)
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

    fun unpackOption u ins =
      (u >> SOME || (fn ins => (NONE, ins))) ins
  
    fun unpackPair (u1, u2) ins =
      let
        val (v1, ins1) = u1 ins
        val (v2, ins2) = u2 ins1
      in
        ((v1, v2), ins2)
      end
  
    fun unpackTuple3 (u1, u2, u3) ins =
      let
        val (v1, ins1) = u1 ins
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
      in
        ((v1, v2, v3), ins3)
      end
  
    fun unpackTuple4 (u1, u2, u3, u4) ins =
      let
        val (v1, ins1) = u1 ins
        val (v2, ins2) = u2 ins1
        val (v3, ins3) = u3 ins2
        val (v4, ins4) = u4 ins3
      in
        ((v1, v2, v3, v4), ins4)
      end
  
    fun unpackTuple5 (u1, u2, u3, u4, u5) ins =
      let
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
  end
end

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

structure MessagePackBinIO = MessagePack(structure S = BinIO.StreamIO)
structure MessagePackBinTextIO = MessagePack(structure S = BinTextIO)

