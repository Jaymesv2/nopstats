{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import Lib
import Parser (parseChapter)
import Reddit.Types
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy as B
import Data.Aeson 
import Data.Aeson.Types
import Data.Either 
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Time
import Control.Monad
import System.Directory
import Text.Printf
import Control.Lens hiding (children)

makeRedditRequest :: IO (Response ByteString)
makeRedditRequest = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "https://www.reddit.com/user/SpacePaladin15/submitted/.json?limit=100"
    httpLbs request manager

getCache :: FilePath -> NominalDiffTime ->  IO (Maybe Value)
getCache path time = doesFileExist path >>= guard >> liftM2 diffUTCTime (getModificationTime path) getCurrentTime >>=  guard . (<time) >>  decodeFileStrict path
{-getCache path time = doesFileExist path >>= \exists -> if exists
            then liftM2 diffUTCTime (getModificationTime path) getCurrentTime >>= guard . (<time) >> decode <$> B.readFile path
            else pure Nothing-}

-- updates cache if nessessary, returns nothing if things fail
-- TODO this should just throw an exception if it fails
getData :: FilePath -> NominalDiffTime -> IO (Maybe Value)
getData cachePath time = getCache cachePath time >>= \case 
    Nothing -> makeRedditRequest >>= \response -> if responseStatus response == status200 
        then case decode (responseBody response) of
            Just obj -> B.writeFile cachePath (encode obj) >> pure (Just obj)
            Nothing -> pure Nothing
        else pure Nothing
    Just a -> pure (Just a)

parseRedditResponse :: Value -> Maybe (Thing (Listing Link))
parseRedditResponse = parseMaybe parseJSON

main :: IO ()
main = do
    val <- getData "cache.json" 1800
    case val >>= parseRedditResponse  of
        Nothing -> print ("failed to parse" :: String)
        Just posts -> do
            let chapters = rights $ fmap (parseChapter . (^. data')) (posts ^. (data' . children))
                totalWords = fromIntegral (sum $ fmap (\c -> length $ T.words $ c ^. text) chapters) :: Double
                wordsPerPage = 300 :: Double
                duneWordCount = 188000 :: Double
                numChapters = fromIntegral $ length chapters :: Double
                avgWordCountPerChapter = totalWords / numChapters
                perspectiveFreq = frequency . fmap (^. perspective) $ chapters
            B.writeFile "out.json" $ encode chapters
            writeFile "totals.csv" $ makeLengthsCsv chapters
            writeFile "perspectives.csv" $ makePerspectivesCsv chapters
            writeFile "word_counts.csv" $ makeWordCountCsv chapters

            printf "Total length: %.0f words (%f pages), %.2f%% of Dune\n" totalWords (totalWords/ wordsPerPage) ((totalWords / duneWordCount)*100)
            printf "Average per chapter: %.0f words (%.1f pages)\n\n" avgWordCountPerChapter (totalWords / wordsPerPage)
            forM_ perspectiveFreq (\(p, n) -> printf "%s (%.2f%%)\n" p ((fromIntegral n / numChapters)*100))