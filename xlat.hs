import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Clipboard (setClipboardString)
  
main :: IO ()
main = do 
 choice <- fmap ((flip elem ru) . head) args
 str <- fmap (map (toRu $ makeMap choice)) args
 setClipboardString str
 putStrLn str
 where
 args :: IO String
 args = fmap unwords getArgs
 ru, en :: String
 ru = "`~!@#$%^&*()_+qwertyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>?" 
 en = "ёЁ!\"№;%:?*()_+йцукенгшщзхъфывапролджэячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,"
 toRu :: Map.Map Char Char -> Char -> Char
 toRu m e = if Map.member e m then m Map.! e else e
 makeMap :: Bool -> Map.Map Char Char
 makeMap b = Map.fromList $ if b then zip ru en else zip en ru
  