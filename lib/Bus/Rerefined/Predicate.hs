{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Bus.Rerefined.Predicate (NotEmpty, Trimmed, ValidPath, NetworkPort) where

import Data.Char (isSpace)
import Data.String (IsString (fromString))
import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import Data.Text.Builder.Linear (fromDec, fromText)
import Rerefined.Predicate (Predicate (PredicateName), Refine (validate))
import Rerefined.Predicate.Common (validateFail)
import System.OsPath (OsPath, decodeUtf, isValid)

data Trimmed

instance Predicate Trimmed where
    type PredicateName d Trimmed = "Trimmed"

instance Refine Trimmed Text where
    validate p = \case
        Empty -> Nothing
        str@(x :< xs) -> do
            let err = validateFail p ("String: " <> fromText str) []

            if isSpace x
                then err
                else case xs of
                    Empty -> Nothing
                    (_ :> x') ->
                        if isSpace x'
                            then err
                            else Nothing

data NotEmpty

instance Predicate NotEmpty where
    type PredicateName d NotEmpty = "NotEmpty"

instance Refine NotEmpty Text where
    validate p = \case
        Empty -> validateFail p "Empty string" []
        _ -> Nothing

data ValidPath

instance Predicate ValidPath where
    type PredicateName d ValidPath = "ValidPath"

instance Refine ValidPath OsPath where
    validate p path =
        if isValid path
            then Nothing
            else validateFail p ("Invalid path: " <> path') []
      where
        path' = fromString $ case decodeUtf path of
            Just fp -> fp
            Nothing -> show path

data NetworkPort

instance Predicate NetworkPort where
    type PredicateName d NetworkPort = "NetworkPort"

instance Refine NetworkPort Int where
    validate p num =
        if num < 0 || num > 65535
            then validateFail p ("Invalid port: " <> fromDec num) []
            else Nothing
