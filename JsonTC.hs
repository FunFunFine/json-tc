module JsonTC  where
import Data.List (intercalate)
import Text.Printf (printf)

data JSON = JsonFloat Float
            | JsonString String 
            | JsonArray [JSON]
            | JsonObject [(String, JSON)] --[("a", ...), ("a", ...)]
            | JsonBool Bool 
            | JsonNull

class FromJSON a where
    fromJSON :: JSON -> Maybe a

class ToJSON a where
    toJSON :: a -> JSON


instance Show JSON where
    show = prettyPrint' 0 
        where
            prettyPrint' :: Int -> JSON -> String
            prettyPrint' count (JsonFloat x) = if (x - fromInteger (truncate x)) == 0  then show $ truncate x else show x
            prettyPrint' count (JsonString x) = show x
            prettyPrint' count (JsonArray x) = "[" ++ "\n" ++ doubleNLine (count+1) " " ++ intercalate ("," ++ "\n" ++ doubleNLine (count+1) " ") (map (prettyPrint' (count+1)) x) ++ "\n" ++ doubleNLine count " " ++ "]"
            prettyPrint' count (JsonObject x) = printf "{" ++ intercalate "," (map (\y -> "\n" ++ doubleNLine (count+1) " " ++ show (fst y) ++ ": " ++ prettyPrint' (count+1) (snd y)) x) ++ "\n" ++ doubleNLine count " " ++ "}"
            prettyPrint' count (JsonBool False) = "false"
            prettyPrint' count (JsonBool True) = "true"
            prettyPrint' count JsonNull = "null"

            doubleNLine :: Int -> String -> String
            doubleNLine 0 _ = "" 
            doubleNLine count line = line ++ line ++ doubleNLine (count-1) line

