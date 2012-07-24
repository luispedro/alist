module Data.AList
    ( AList
    , singleton
    , fromList
    , Fld.toList
    , safeHead
    , safeTail
    , head
    , tail
    , length
    ) where

import Prelude hiding (head, tail, length)
import Data.Monoid
import qualified Data.Foldable as Fld
import Data.Functor
import Data.Traversable
import Data.Maybe

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

safeHead :: AList a -> Maybe a
safeHead ALEmpty = Nothing
safeHead (ALSingleton a) = Just a
safeHead (ALAppend left right) = safeHead left <|> safeHead right

head :: AList a -> a
head = fromJust . safeHead

safeTail :: AList a -> Maybe a
safeTail ALEmpty = Nothing
safeTail (ALSingleton a) = Just a
safeTail (ALAppend left right) = safeTail right <|> safeTail left

tail :: AList a -> a
tail = fromJust . safeTail

length :: AList a -> Int
length ALEmpty = 0
length (ALSingleton _) = 1
length (ALAppend left right) = length left + length right

