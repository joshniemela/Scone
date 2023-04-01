import Parser
import Val
import Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Except

-- Read file and parse it
main :: IO ()
main = do
    contents <- readFile "test.phl"
    let parsed = readExprFile $ T.pack contents
    case parsed of
        Left err -> putStrLn $ errorBundlePretty err
        Right val -> print val
