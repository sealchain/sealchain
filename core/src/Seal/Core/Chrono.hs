{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Chronological sequences.

module Seal.Core.Chrono
       ( Chrono
       , NewestFirst(..)
       , _NewestFirst
       , OldestFirst(..)
       , _OldestFirst
       , toNewestFirst
       , toOldestFirst
       , NE
       , nonEmptyOldestFirst
       , nonEmptyNewestFirst
       , splitAtNewestFirst
       , splitAtOldestFirst
       ) where

import           Universum

import           Control.Lens (makePrisms, makeWrapped, _Wrapped)
import qualified Control.Lens as Lens (Each (..))
import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup (Semigroup)
import qualified GHC.Exts as IL

import           Seal.Binary.Class (Bi)

newtype NewestFirst f a = NewestFirst {getNewestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container,
            Bi,
            NFData)
newtype OldestFirst f a = OldestFirst {getOldestFirst :: f a}
  deriving (Eq, Ord, Show,
            Functor, Foldable, Traversable,
            Container,
            Bi,
            NFData)

makePrisms  ''NewestFirst
makeWrapped ''NewestFirst

makePrisms  ''OldestFirst
makeWrapped ''OldestFirst

deriving instance Semigroup (f a) => Semigroup (NewestFirst f a)
deriving instance Semigroup (f a) => Semigroup (OldestFirst f a)

instance Lens.Each (f a) (f b) a b =>
         Lens.Each (NewestFirst f a) (NewestFirst f b) a b where
    each = _Wrapped . Lens.each
instance Lens.Each (f a) (f b) a b =>
         Lens.Each (OldestFirst f a) (OldestFirst f b) a b where
    each = _Wrapped . Lens.each

instance One (f a) => One (NewestFirst f a) where
    type OneItem (NewestFirst f a) = OneItem (f a)
    one = NewestFirst . one
instance One (f a) => One (OldestFirst f a) where
    type OneItem (OldestFirst f a) = OneItem (f a)
    one = OldestFirst . one

instance IL.IsList (f a) => IL.IsList (NewestFirst f a) where
    type Item (NewestFirst f a) = IL.Item (f a)
    toList = IL.toList . getNewestFirst
    fromList = NewestFirst . IL.fromList

instance IL.IsList (f a) => IL.IsList (OldestFirst f a) where
    type Item (OldestFirst f a) = IL.Item (f a)
    toList = IL.toList . getOldestFirst
    fromList = OldestFirst . IL.fromList

class Chrono f where
    toNewestFirst :: OldestFirst f a -> NewestFirst f a
    toOldestFirst :: NewestFirst f a -> OldestFirst f a

instance Chrono [] where
    toNewestFirst = NewestFirst . reverse . getOldestFirst
    toOldestFirst = OldestFirst . reverse . getNewestFirst

instance Chrono NonEmpty where
    toNewestFirst = NewestFirst . NE.reverse . getOldestFirst
    toOldestFirst = OldestFirst . NE.reverse . getNewestFirst

type NE = NonEmpty

nonEmptyOldestFirst ::
    forall a.
       OldestFirst [] a
    -> Maybe (OldestFirst NE a)
nonEmptyOldestFirst = coerce (nonEmpty @a)

nonEmptyNewestFirst ::
    forall a.
       NewestFirst [] a
    -> Maybe (NewestFirst NE a)
nonEmptyNewestFirst = coerce (nonEmpty @a)

splitAtOldestFirst ::
    forall a.
       Int
    -> OldestFirst NE a
    -> (OldestFirst [] a, OldestFirst [] a)
splitAtOldestFirst = coerce (NE.splitAt @a)

splitAtNewestFirst ::
    forall a.
       Int
    -> NewestFirst NE a
    -> (NewestFirst [] a, NewestFirst [] a)
splitAtNewestFirst = coerce (NE.splitAt @a)
