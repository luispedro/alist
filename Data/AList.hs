module Data.AList
    ( AList
    , singleton
    , fromList
    , Fld.toList
    ) where

import Data.Monoid
import qualified Data.Foldable as Fld
import Data.Functor
import Data.Traversable

import Control.Applicative

data AList a = ALEmpty
                | ALSingleton a
                | ALAppend (AList a) (AList a)
                deriving (Eq, Show)


instance Functor AList where
    fmap _ ALEmpty = ALEmpty
    fmap f (ALSingleton a) = ALSingleton (f a)
    fmap f (ALAppend left right) = ALAppend (f `fmap` left) (f `fmap` right)

instance Monoid (AList a) where
    mempty = ALEmpty
    mappend = ALAppend

instance Fld.Foldable AList where
    foldMap _ ALEmpty = mempty
    foldMap f (ALSingleton a) = f a
    foldMap f (ALAppend left right) = Fld.foldMap f left `mappend` Fld.foldMap f right

    foldr _ z ALEmpty = z
    foldr f z (ALSingleton a) = f a z
    foldr f z (ALAppend left right) = Fld.foldr f (Fld.foldr f z right) left

instance Traversable AList where
    traverse _ ALEmpty = pure ALEmpty
    traverse f (ALSingleton a) = ALSingleton <$> f a
    traverse f (ALAppend left right) = ALAppend <$> traverse f left <*> traverse f right

singleton :: a -> AList a
singleton = ALSingleton

fromList :: [a] -> AList a
fromList [] = ALEmpty
fromList (a:as) = singleton a `ALAppend` fromList as

