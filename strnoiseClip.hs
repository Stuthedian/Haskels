import Data.Char (toUpper)
import System.Random (randomRIO)
import System.Clipboard (setClipboardString, getClipboardString)
import Control.Monad (join)

main :: IO ()
main = do
 join $ fmap (test donoise) getClipboardString
 where
 test :: (String -> IO ()) -> (Maybe String) -> IO ()
 test _ Nothing = return ()
 test f (Just s) = f s
 donoise :: String -> IO ()
 donoise s = do
  capsed <- (sequence $ map randCap s)
  setClipboardString capsed
 randCap :: Char -> IO Char
 randCap x = fmap ($ x) $ fmap gate $ fmap ((==) 0) should
 should :: IO Int
 should = randomRIO (0,1)
 gate :: Bool -> (Char -> Char)
 gate x = if x then toUpper else id