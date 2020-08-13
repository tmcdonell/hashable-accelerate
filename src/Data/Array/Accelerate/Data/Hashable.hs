{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Hashable
-- Copyright   : [2020] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module defines a class 'Hashable', for types that can be converted
-- to a hash value.
--

module Data.Array.Accelerate.Data.Hashable (

  Hashable(..),
  hashUsing, defaultHashWithSalt,

) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Data.Either
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Ratio

import Prelude                                            ( (<$>), id, concat )
import Control.Monad                                      ( mapM )
import Language.Haskell.TH                                hiding ( Exp, match )
import qualified Prelude                                  as P
import qualified Data.Bits                                as P
import qualified Data.List                                as P

#include "MachDeps.h"

infixl 0 `hashWithSalt`

-- | The class of types that can be converted to a hash value.
--
class Elt a => Hashable a where
    -- | Return a hash value for the argument, using the given salt.
    --
    -- The general contract of 'hashWithSalt' is:
    --
    --  * If two values are equal according to the '==' method, then
    --    applying the 'hashWithSalt' method on each of the two values
    --    /must/ produce the same integer result if the same salt is
    --    used in each case.
    --
    --  * It is /not/ required that if two values are unequal
    --    according to the '==' method, then applying the
    --    'hashWithSalt' method on each of the two values must produce
    --    distinct integer results. However, the programmer should be
    --    aware that producing distinct integer results for unequal
    --    values may improve the performance of hashing-based data
    --    structures.
    --
    --  * This method can be used to compute different hash values for
    --    the same input by providing a different salt in each
    --    application of the method. This implies that any instance
    --    that defines 'hashWithSalt' /must/ make use of the salt in
    --    its implementation.
    --
    hashWithSalt :: Exp Int -> Exp a -> Exp Int

    -- | Like 'hashWithSalt', but no salt is used. The default
    -- implementation uses 'hashWithSalt' with some default salt.
    -- Instances might want to implement this method to provide a more
    -- efficient implementation than the default implementation.
    --
    hash :: Exp a -> Exp Int
    hash = hashWithSalt defaultSalt

-- | Transform a value into a 'Hashable' value, then hash the transformed
-- value using the given salt.
--
-- This is a useful shorthand in cases where a type can easily be mapped to
-- another type that is already an instance of 'Hashable'.
--
hashUsing
    :: Hashable b
    => (Exp a -> Exp b)     -- ^ transformation function
    -> Exp Int              -- ^ salt
    -> Exp a                -- ^ value to transform
    -> Exp Int
hashUsing f salt x = hashWithSalt salt (f x)

-- | A default salt used in the implementation of 'hash'.
--
defaultSalt :: Exp Int
#if WORD_SIZE_IN_BITS == 64
defaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4
#else
defaultSalt = 0x087fc72c
#endif

defaultHashWithSalt :: Hashable a => Exp Int -> Exp a -> Exp Int
defaultHashWithSalt salt x = salt `combine` hash x

-- | Combine two given hash values. 'combine' has zero as a left identity.
--
combine :: Exp Int -> Exp Int -> Exp Int
combine h1 h2 = (h1 * 16777619) `xor` h2

instance Hashable Int where
  hash         = id
  hashWithSalt = defaultHashWithSalt

instance Hashable Int8 where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Int16 where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Int32 where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Int64 where
  hash x
    | P.finiteBitSize (undefined :: Int) P.== 64 = fromIntegral x
    | otherwise                                  = fromIntegral (fromIntegral x `xor`
                                                                (fromIntegral x `shiftR` 32 :: Exp Word64))
  hashWithSalt = defaultHashWithSalt

instance Hashable Word where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Word8 where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Word16 where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Word32 where
  hash         = fromIntegral
  hashWithSalt = defaultHashWithSalt

instance Hashable Word64 where
  hash x
    | P.finiteBitSize (undefined :: Int) P.== 64 = fromIntegral x
    | otherwise                                  = fromIntegral (x `xor` (x `shiftR` 32))
  hashWithSalt = defaultHashWithSalt

instance Hashable () where
  hash _       = constant (P.fromEnum ())
  hashWithSalt = defaultHashWithSalt

instance Hashable Bool where
  hash         = boolToInt
  hashWithSalt = defaultHashWithSalt

instance Hashable Char where
  hash         = ord
  hashWithSalt = defaultHashWithSalt

instance Hashable Half where
  hash x =
    if x == 0.0 || x == -0.0
       then 0
       else hash (bitcast x :: Exp Word16)
  hashWithSalt = defaultHashWithSalt

instance Hashable Float where
  hash x =
    if x == 0.0 || x == -0.0
       then 0
       else hash (bitcast x :: Exp Word32)
  hashWithSalt = defaultHashWithSalt

instance Hashable Double where
  hash x =
    if x == 0.0 || x == -0.0
       then 0
       else hash (bitcast x :: Exp Word64)
  hashWithSalt = defaultHashWithSalt

instance Hashable a => Hashable (Complex a) where
    hash (r ::+ i)           = hash r `hashWithSalt` i
    hashWithSalt s (r ::+ i) = s `hashWithSalt` r `hashWithSalt` i

instance Hashable a => Hashable (Ratio a) where
  hash a           = hash (numerator a) `hashWithSalt` denominator a
  hashWithSalt s a = s `hashWithSalt` numerator a `hashWithSalt` denominator a

-- | A value with bit pattern (01)* (or 5* in hexa), for any size of Int.
-- It is used as data constructor distinguisher.
--
distinguisher :: Exp Int
distinguisher = fromIntegral $ (maxBound :: Exp Word) `quot` 3

instance Hashable a => Hashable (Maybe a) where
  hash = match \case
    Nothing_ -> 0
    Just_ x  -> distinguisher `hashWithSalt` x
  hashWithSalt = defaultHashWithSalt

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hash = match \case
    Left_ x  -> 0             `hashWithSalt` x
    Right_ x -> distinguisher `hashWithSalt` x
  hashWithSalt = defaultHashWithSalt

$(runQ $
    let
        tupT :: [TypeQ] -> TypeQ
        tupT tup =
          let n = P.length tup
           in P.foldl' (\ts t -> [t| $ts $t |]) (tupleT n) tup

        mkTup :: Int -> Q [Dec]
        mkTup n =
          let
              xs  = [ mkName ('x':P.show i) | i <- [0 .. n-1] ]
              ctx = tupT (P.map (\x -> [t| Hashable $(varT x) |]) xs)
              res = tupT (P.map varT xs)
              pat = conP (mkName ('T':P.show n)) (P.map varP xs)
          in
          [d| instance $ctx => Hashable $res where
                hash $pat = $(P.foldl' (\vs v -> [| $vs `hashWithSalt` $v |]) [| hash $(varE (P.head xs))|] (P.map varE (P.tail xs)))
                hashWithSalt = defaultHashWithSalt
            |]

    in
    concat <$> mapM mkTup [2..16]
 )

