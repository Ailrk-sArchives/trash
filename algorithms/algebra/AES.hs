{-# LANGUAGE BinaryLiterals #-}

module AES where

import Data.Bits
import Data.Maybe
import Data.List

-- baby AES
--
-- AES is designed to replace DES.
-- DES had US government involved, and the implementation was not transparent,
-- which makes the security of the cryptosystem questionable.
-- Also DES has a small number of possible key, so it's actually vulnerable over
-- exthausted key search.
-- while AES has public implementation that everybody can study it. Also AES has
-- much larger number of keys, which makes it much more secure.
-- So far there is no report of successful attack againt AES.

-- A baby AES: simple ASE that you can write in couple lines.
-- Steps:
-- BytesSub.
--  A nonlinear substitution is performed on each entry of the matrix. This is constructed
--  by first computing the inverse of each entry as an element of F256, and then temporarily
--  treating each entry as a vector in Z(8, 2) and performing an affine transformation on it.
--  this is the only part of the encryption algorithm which is not just a computation over
--  F256, which helps break up the simple algebraic structure of the cryptosystem and makes it
--  resistant to certain kinds of cryptanalytic attacks.
--
-- ShiftRows.
--  The entries in each row are cyclically shifted a certain number of spaces to the right
--  this is done because all of other steps are only performed in individual columns, so this
--  step breaks up and mixes columns together.
--
-- MixColumns.
--  This is the big diffusion step. The 4x4 matrix is multiplied by another fixed matrix.
--  with all computation over F256.
--
-- AddRoundKey.
--  4x4 round key matrix is derived from theecret master key and just added (bitwise) to the
--  matrix
--
-- The process is repeated in 10 rounds to finally encrypt the block.

-- baby AES

-- field table with p(x) = x^4 + x + 1
fieldTableF16 =
  [ 0b0001,
    0b0010,
    0b0100,
    0b1000,
    0b0011,
    0b0110,
    0b1100,
    0b1011,
    0b0101,
    0b1010,
    0b0111,
    0b1110,
    0b1111,
    0b1101,
    0b1001
  ]

byteSubM =
  [ [1, 0, 1, 1],
    [1, 1, 0, 1],
    [1, 1, 1, 0],
    [0, 1, 1, 1]
  ]
