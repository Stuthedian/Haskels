import qualified Data.Map as Map
import System.Clipboard (setClipboardString, getClipboardString)
import Control.Monad (join)

main :: IO ()
main = do
 join $ fmap (test doxlat) getClipboardString
 where
 test :: (String -> IO ()) -> (Maybe String) -> IO ()
 test _ Nothing = return ()
 test f (Just s) = f s
 doxlat :: String -> IO ()
 doxlat s = do
  let _map = makeMap $ choice s
  setClipboardString $ map (toRu _map) s
 choice :: String -> Bool
 choice = (flip elem ru) . head
 ru, en :: String
 ru = "`~!@#$%^&*()_+qwertyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>?" 
 en = "ёЁ!\"№;%:?*()_+йцукенгшщзхъфывапролджэячсмитьбю.ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ,"
 toRu :: Map.Map Char Char -> Char -> Char
 toRu m e = if Map.member e m then m Map.! e else e
 makeMap :: Bool -> Map.Map Char Char
 makeMap b = Map.fromList $ if b then zip ru en else zip en ru
  