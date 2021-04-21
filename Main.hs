{-# LANGUAGE InstanceSigs #-}

module Main where

import JsonTC

main :: IO ()
main = do
    print jsonHumans
    print humans



data Human = Man {mName :: String, age :: Int} 
             | Woman {wName :: String, amountOfKids :: Int} 
             deriving (ToJSON, FromJSON)

instance ToJSON Human where
  toJSON (Man n a) =
    JsonObject
      [ ("name", JsonString n),
        ("age", JsonFloat . fromIntegral $ a)
      ]
  toJSON (Woman n a) =
    JsonObject
      [ ("name", JsonString n),
        ("amountOfKids", JsonFloat . fromIntegral $a)
      ]

-- {
--  "name" : "Pyotr",
--   "age" : 33
--}
instance FromJSON Human where
  fromJSON :: JSON -> Maybe Human
  fromJSON (JsonObject [("name", JsonString n), (key, JsonFloat val)]) =
    case key of
      "age" -> Just . Man n $ round val
      "amountOfKids" -> Just . Woman n $ round val
      _ -> Nothing
  fromJSON _ = Nothing

prints = [putStr "hello", putChar 'a', putStrLn "b"]

print' = sequence prints

instance FromJSON a => FromJSON [a] where
  fromJSON :: JSON ->  Maybe [a]
  fromJSON (JsonArray xs) =  traverse fromJSON xs
  fromJSON _ = Nothing

man :: Human
man = Man "Pyotr" 33

woman = Woman "Natasha" 0

jsonHumans :: JSON
jsonHumans = JsonArray . map toJSON $ [man, woman]

humans :: Maybe [Human]
humans = fromJSON jsonHumans
