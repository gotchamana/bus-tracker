module Bus.Util.Aeson (fieldPrefixRemovalOptions) where

import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.Char (toLower)
import Data.List (stripPrefix)

fieldPrefixRemovalOptions :: String -> Options
fieldPrefixRemovalOptions fieldPrefix =
    defaultOptions
        { fieldLabelModifier = removePrefix
        }
  where
    removePrefix field = case stripPrefix fieldPrefix field of
        Just result -> case result of
            [] -> []
            (x : xs) -> toLower x : xs
        Nothing -> field
