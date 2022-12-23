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
import Reddit.Types.Lens
import Control.Lens hiding (noneOf)

parseChapter :: Link -> Either ParseError Chapter
parseChapter l = unless ("The Nature of Predators" `T.isPrefixOf` (l ^. title)) (Left $ newErrorMessage (Expect "Not a valid chapter") (newPos (T.unpack $ l ^. title) 0 0)) >>
                 runParser (chapterP chapterNum) () (T.unpack $ l ^. title) (l ^. selftext) <*> pure l
                    where 
                        chapterNum = if chapterNumStr == "" then 1 else read $ T.unpack chapterNumStr
                        chapterNumStr = T.drop 24 (l ^. title)

chapterP :: Int -> Parsec Text () (Link -> Chapter)
chapterP chp = do
    firstChar <- anyChar
    when (firstChar /= '*') $ skipMany (noneOf "\r\n") <* string "\n\n\\---\n\n"
    rawPerspective <- string "*" *> manyTill anyChar (try $ string "***\n\n")
    rawDate <- string "*" *> manyTill anyChar (try $ string "*\n\n")
    text <- T.pack <$> manyTill anyChar (try $ string "\\---\n\n")
    let perspective = T.pack . drop 30 . filter (/= '*') $ rawPerspective
        perspective' = perspective{-if T.elem ',' perspective || head perspective == "G"
            then T.takeWhile (/=',') perspective
            else last . T.words $ perspective-}

    pure $ Chapter chp perspective' (T.pack . drop 2 . dropWhile (/= ':') . filter (/= '*') $ rawDate) text

--cleanupHtml:: Text -> Text
--cleanupHtml = T.replace "&lt;" "<" . T.replace "&gt;" ">"