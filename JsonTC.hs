{-# LANGUAGE InstanceSigs #-}

-- чтобы можно было писать сигнатуры в определении инстанса

module JsonTC
  ( ToJSON (toJSON),
    FromJSON (fromJSON),
    JSON (..),
  )
where

import Data.List (intercalate)
import Text.Printf (printf)

data JSON
  = JsonFloat Float
  | JsonString String
  | JsonArray [JSON]
  | JsonObject [(String, JSON)] --[("a", ...), ("a", ...)]
  | JsonBool Bool
  | JsonNull

data DecodingError = DecodingError String

class FromJSON a where
  fromJSON :: JSON -> Either DecodingError a

class ToJSON a where
  toJSON :: a -> JSON

instance FromJSON a => FromJSON [a] where
  fromJSON :: JSON -> Either DecodingError [a]
  fromJSON (JsonArray xs) = traverse fromJSON xs
  fromJSON _ = DecodingError "Invalid: not a list"

instance Show JSON where
  show = prettyPrint' 0
    where
      space = " "
      tab = space ++ space
      prettyPrint' :: Int -> JSON -> String
      prettyPrint' depth json = case json of
        (JsonFloat x) -> if (x - fromInteger (truncate x)) == 0 then show $ truncate x else show x
        (JsonString x) -> show x
        (JsonArray array) ->
          "[\n"
            ++ nextIndent
            ++ (intercalate ("," ++ "\n" ++ nextIndent) . map (prettyPrint' (depth + 1)) $ array)
            ++ "\n"
            ++ nextIndent
            ++ "]"
        (JsonObject object) ->
          "{"
            ++ ( intercalate ","
                   . map
                     ( \(key, value) ->
                         printf "\n%s%s: %s" nextIndent (show key) (prettyPrint' (depth + 1) value)
                     )
                   $ object
               )
            ++ "\n"
            ++ currentIndent
            ++ "}"
        (JsonBool False) -> "false"
        (JsonBool True) -> "true"
        JsonNull -> "null"
        where
          indent n = concat . replicate n $ tab
          currentIndent = indent depth
          nextIndent = indent $ depth + 1
