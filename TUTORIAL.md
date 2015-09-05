ML-MessagePack Tutorial
=======================

This tutorial shows basic usage of ML-MessagePack.

## Preliminaries

The core of ML-MessagePack is MessagePack functor, which takes an argument structure for I/O.
You can pack values to and unpack values from anything as long as a structure with the following signature is provided.

    sig
      type instream
      type outstream
      val input1 : instream -> (Word8.word * instream) option
      val inputN : instream * int -> Word8Vector.vector * instream
      val output : outstream * Word8Vector.vector -> unit
      val output1 : outstream * Word8.word -> unit
    end

In this tutorial, we use BytesIO structure. It is bundled with ML-MessagePack and can read from and write to Word8Vector.vector.
ML-MessagePack also provides MessagePackBytesIO structure which is defined as follows.

    structure MessagePackBytesIO = MessagePack(BytesIO)

The examples in the tutorial are taken from the interactive shell of Poly/ML.

## Packing primitive values

To pack a value, you need a packer of appropriate type.
Since packers for primitive types are included in MessagePackBytesIO.Pack structure, it is useful to open the struture.

    > open MessagePackBytesIO.Pack;

To pack an integer, type:

    > val outs = BytesIO.mkOutstream ();
    > doPack packInt 1 outs;

Then, pack a boolean value:

    > doPack packBool true outs;

Written data can be obtained by BytesIO.toBytes function.

    > BytesIO.toBytes outs;
    val it = fromList[0wx1, 0wxC3] : Word8Vector.vector

The values 0x01 and 0xC3 are serialized MessagePack representations of 1 and true respectively.

Other primitive packers such as packUnit, packReal, packString and packBytes can be used similarly.

## Packing complex values

To pack a value of complex type such as tuple or list, you need to compose a packer for it.

To pack a pair (a tuple of two values), use packPair combinator.

    > val outs = BytesIO.mkOutstream ();
    > doPack (packPair (packInt, packBool)) (1, true) outs;
    > BytesIO.toBytes outs;
    val it = fromList[0wx92, 0wx1, 0wxC3] : Word8Vector.vector

packPair takes two packers as arguments and creates a new packer which packs a tuple of two values.
packTuple3 ... packTuple6 are used similarly.
Note that tuples are mapped to Array type of MessagePack.

To pack a list, type:

    > val outs = BytesIO.mkOutstream ();
    > doPack (packList packInt) [1, 2, 3] outs;
    > BytesIO.toBytes outs;
    val it = fromList[0wx93, 0wx1, 0wx2, 0wx3] : Word8Vector.vector

packVector and packArray are similar to this. They all are mapped to Array type of MessagePack.

These combinatory packers can be used in conjunction to pack more complex types such as (int * bool) list:

    > val outs = BytesIO.mkOutstream ();
    > doPack (packList (packPair (packInt, packBool))) [(1, true),  (2, false)] outs;
    > BytesIO.toBytes outs;
    val it = fromList[0wx92, 0wx92, 0wx1, 0wxC3, 0wx92, 0wx2, 0wxC2]
    : Word8Vector.vector

ML-MessagePack maps ('a * 'b) list to Map type of MessagePack:

    > val outs = BytesIO.mkOutstream ();
    > doPack (packPairList (packInt, packBool)) [(1, true),  (2, false)] outs;
    > BytesIO.toBytes outs;
    val it = fromList[0wx82, 0wx1, 0wxC3, 0wx2, 0wxC2] : Word8Vector.vector

Be warned that `packList (packPair (packInt, packBool))` and `packPairList (packInt, packBool)` are not same even though they both are of type `(int * bool) list packer`.
The former produces Array of MessagePack and the latter produces Map of MessageType.

## Unpacking values

Unpacking is very similar to packing. Let us first open MessagePackBytesIO.Unpack structure.

    > open MessagePackBytesIO.Unpack;

To unpack an integer:

    > doUnpack unpackInt (BytesIO.fromBytes (Word8Vector.fromList [0wx01]));
    val it = (1, fromList[]) : int * BytesIO.instream

doPack returns a pair. The first value of the pair is the unpacked value and the second value is the rest of the input stream.

To unpack (int * bool) list:

    > doUnpack (unpackList (unpackPair (unpackInt, unpackBool)))
    # (BytesIO.fromBytes (Word8Vector.fromList [0wx92, 0wx92, 0wx1, 0wxC3, 0wx92, 0wx2, 0wxC2]));
    val it = ([(1, true), (2, false)], fromList[])
    : (int * bool) list * BytesIO.instream

## Alternative unpacking

ML-MessagePack provides two combinators for unpacking bytes: unpackBytes and unpackBytesFromStr.
While the former produces Word8Vector.vector from Binary type of MessagePack, the latter does so from String type of MessagePack.
Now suppose that you are writing an application that read from a MessagePack stream to which the sender may send either Binary or String,
and you want to treat the data as Word8Vector.vector anyway.

In such situation, || combinator is useful:

    > infix 0 ||;
    > val unpackRaw = (unpackBytes || unpackBytesFromStr);
    val unpackRaw = ? : Word8Vector.vector MessagePackBytesIO.Unpack.unpacker
    > doUnpack unpackRaw (BytesIO.fromBytes (Word8Vector.fromList [0wxA1, 0wx41]));
    val it = (fromList[0wx41], fromList[])
    > doUnpack unpackRaw (BytesIO.fromBytes (Word8Vector.fromList [0wxC4, 0wx1, 0wx41]));
    val it = (fromList[0wx41], fromList[])

Here unpackRaw tries unpackBytes first. If unpackBytes fails, it tries unpackBytesFromStr next. 

## Transformation

Let us consider another situation. Sender may send either Integer or Float to the stream.
You might think as folloing:

    > val unpackNumber = (unpackReal || unpackInt);

However, this does not work because of type error; real and int do not unify.

In such situation, >> combinator can be used:

    > infix 3 >>;
    > val unpackNumber = (unpackReal || unpackInt >> Real.fromInt);;
    val unpackNumber = ? : real MessagePackBytesIO.Unpack.unpacker
    > doUnpack unpackNumber (BytesIO.fromBytes (Word8Vector.fromList [0wx01]));
    val it = (1.0, fromList[]) : real * BytesIO.instream
    > doUnpack unpackNumber
    # (BytesIO.fromBytes (Word8Vector.fromList [0wxcb, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00, 0wx00]));
    val it = (0.0, fromList[]) : real * BytesIO.instream

The combinator takes 'a unpacker and a function of ('a -> 'b) and makes 'b unpacker.
Since unpackInt is an int unpacker and Real.fromInt is of type `int -> real`, `unpackInt >> Real.fromInt` is a real unpacker.

## Online processing

Suppose that your task is to read integers from a stream and calculate sum of them.
One way to do this would be:

    > val (ints, _) = doUnpack (unpackList unpackInt) (BytesIO.fromBytes (Word8Vector.fromList [0wx93, 0wx1, 0wx2, 0wx3]));
    val ints = [1, 2, 3] : int list
    > List.foldl (fn (i, sum) => i + sum) 0 ints;
    val it = 6 : int

This is fine, but what if number of integers is very large?
unpackList constructs a large list first. This may be very memory consuming.

For such cases, ML-MessagePack provides a way to process Array online:

    > val unpackIntSum = unpackArrayFold unpackInt (fn (i, sum) => i + sum) 0;
    val unpackIntSum = ? : int MessagePackBytesIO.Unpack.unpacker
    > doUnpack unpackIntSum (BytesIO.fromBytes (Word8Vector.fromList [0wx93, 0wx1, 0wx2, 0wx3]));
    val it = (6, fromList[]) : int * BytesIO.instream

Here unpackIntSum reads integers from the stream using unpackInt, and applies given function repeatedly to the integers.
