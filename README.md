ML-MessagePack
==============

MessagePack implementation for Standard ML (SML)

Features
--------

- Portable: Depends only on the required components of the SML Basis Library specification.
- Composable: Encoder and decoder are composable by combinators.

Usage
-----

#### MLton and MLKit

Include mlmsgpack.mlb in your MLB file.

#### Poly/ML

From the interactive shell, use .sml files in the following order.

- mlmsgpack-aux.sml
- realprinter-default.sml
- mlmsgpack.sml

#### SML/NJ

Use mlmsgpack.cm.

#### Moscow ML

From the interactive shell, use .sml files in the following order.

- large.sml
- mlmsgpack-aux.sml
- realprinter-fail.sml
- mlmsgpack.sml

Makefile.mosml is also provided.

#### HaMLet

From the interactive shell, use .sml files in the following order.

- mlmsgpack-aux.sml
- realprinter-fail.sml
- mlmsgpack.sml

#### Alice ML

From the interactive shell, use .sml files in the following order.

- mlmsgpack-aux.sml
- realprinter-fail.sml
- mlmsgpack.sml

#### SMLSharp

.smi files are provided. Require mlmsgpack.smi from your .smi file.

Known Problems
--------------

Our recommendation is MLton, MLKit and Poly/ML as all tests passed on them.
SML/NJ and Moscow ML are fine if you don't use real values.

#### SML/NJ

Packing real values fail or produces imprecise results in some cases.

#### Moscow ML

Packing real values is not supported, since some components of the SML Basis Library are not provided.

#### HaMLet

Packing real values is not supported, since some components of the SML Basis Library are not provided.
Some functions are very slow, although they works properly. (We tested HaMLet compiled with MLton.)

#### Alice ML

Packing real values is not supported, since some components of the SML Basis Library are not provided.
Also, some unit tests fail.

#### SMLSharp

Most functions do not work properly because of bugs of SML#.

Status
------

Not complete. Bin and ext are not supported yet.

See Also
--------

There already exists another MessagePack implemenatation for SML, 
called MsgPack-SML, which is targeted for MLton.

https://msgpacksml.codeplex.com/
