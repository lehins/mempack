{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Data.MemPack.Error
-- Copyright   : (c) Alexey Kuleshevich 2024
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Data.MemPack.Error where

import Control.Exception
import Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Typeable
import GHC.Exts

data SomeError where
  SomeError :: (Typeable e, Error e) => e -> SomeError

instance Show SomeError where
  showsPrec p (SomeError e) = showsPrec p e

instance Exception SomeError

-- | Very similar interface to `Exceptions`, except not intended for run time exceptions.
class Show e => Error e where
  toSomeError :: e -> SomeError
  default toSomeError :: Typeable e => e -> SomeError
  toSomeError = SomeError

  fromSomeError :: SomeError -> Maybe e
  default fromSomeError :: Typeable e => SomeError -> Maybe e
  fromSomeError (SomeError t) = cast t

instance Error SomeError where
  toSomeError = id
  fromSomeError = Just

newtype TextError = TextError Text
  deriving newtype (Eq, Show, IsString)

instance Error TextError

instance IsString SomeError where
  fromString = toSomeError . TextError . fromString

newtype ManyErrors = ManyErrors (NonEmpty SomeError)
  deriving (Show)

instance Error ManyErrors

data UnknownError = UnknownError
  deriving (Show)

instance Error UnknownError

fromMultipleErrors :: [SomeError] -> SomeError
fromMultipleErrors es =
  case es of
    [] -> toSomeError UnknownError
    [e] -> e
    e : rest -> toSomeError $ ManyErrors (e :| rest)
