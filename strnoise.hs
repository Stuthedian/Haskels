import Data.Char (toUpper)
import System.Random (randomRIO)
import System.Environment (getArgs)
import System.Clipboard (setClipboardString)

main :: IO ()
main = do 
 capsed <- (sequence . map randCap . unwords) =<< getArgs
 setClipboardString capsed
 putStrLn capsed
 where 
 randCap :: Char -> IO Char
 --randCap x = ($ x) <$> (gate <$> (((==) 0) <$> should))
 randCap x = fmap ($ x) $ fmap gate $ fmap ((==) 0) should
 should :: IO Int
 should = randomRIO (0, 1)
 gate :: Bool -> (Char -> Char)
 gate x = if x then toUpper else id