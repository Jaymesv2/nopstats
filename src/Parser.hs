{-# LANGUAGE OverloadedStrings #-}
module Parser (parseChapter) where
import Lib (Chapter (..))
--import Control.Applicative hiding (optional)
import Control.Monad
import qualified Data.Text  as T
import Data.Text (Text)
import Text.Parsec as P hiding ((<|>))
import Text.Parsec.Error
import Text.Parsec.Pos
import Reddit.Types
import Control.Lens hiding (noneOf)
import Text.Regex.PCRE
import Text.Regex.PCRE.Text ()
import Text.Read (readMaybe)
import Data.Maybe

parseChapter :: Link -> Either ParseError Chapter
parseChapter l = unless ("The Nature of Predators" `T.isPrefixOf` (l ^. title)) (Left $ newErrorMessage (Expect "Not a valid chapter") (newPos (T.unpack $ l ^. title) 0 0)) >>
                 runParser (chapterP (fromMaybe 1 $ readMaybe (T.unpack $ T.drop 24 (l ^. title))) l) () (T.unpack $ l ^. title) (l ^. selftext)

chapterP :: Int -> Link -> Parsec Text () Chapter
chapterP chp link = do
    firstChar <- anyChar
    when (firstChar /= '*') $ skipMany (noneOf "\r\n") <* string "\n\n\\---\n\n"
    rawPerspective <- string "*" *> manyTill anyChar (try $ string "\n\n")
    rawDate <- string "*" *> manyTill anyChar (try $ string "*\n\n")
    text <- T.pack <$> manyTill anyChar (try $ string "\\---\n\n")
    pure $ Chapter chp (extractPerspective . T.pack $ rawPerspective) (T.pack . drop 2 . dropWhile (/= ':') . filter (/= '*') $ rawDate) text link

extractPerspective :: Text -> Text
extractPerspective = T.filter (/=',') . head . (\(_,_,_,x) -> x) . ((=~ perspectiveRegex) :: Text -> (Text, Text, Text, [Text]))
    where perspectiveRegex = "\\*\\*\\*(?>(?>[a-zA-Z-]+ (?!of))*(\\S+)(?>(?>(?> of|,).+\\*\\*\\*)|\\*\\*\\*))" :: Text

--cleanupHtml:: Text -> Text
--cleanupHtml = T.replace "&lt;" "<" . T.replace "&gt;" ">"