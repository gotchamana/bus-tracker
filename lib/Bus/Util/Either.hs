module Bus.Util.Either (maybeToEither, eitherToMaybe) where

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err = \case
    Just a -> Right a
    Nothing -> Left err

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = \case
    Left _ -> Nothing
    Right a -> Just a
