{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, TemplateHaskell, LambdaCase #-}
module Lib  
        ( Chapter (..)
        , makeLengthsCsv
        , makePerspectivesCsv
        , makeWordCountCsv
        , date
        , original_post
        , text
        , chaptersWordFrequency
        , perspective
        , chapter
        , frequency
        ) where
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Reddit.Types
import Control.Lens
import Data.Map as M hiding (foldl')
import Data.List (intercalate, foldl', sortBy)
import Data.Char

data Chapter = Chapter {
    _chapter :: Int,
    _perspective :: Text,
    _date :: Text,
    _text :: Text,
    _original_post :: Link
} deriving (Generic, FromJSON, ToJSON, Show)
makeLenses ''Chapter

makeLengthsCsv :: [Chapter] -> String
makeLengthsCsv = 
    intercalate "\n" . 
    ("Chapter,Word Count":) . 
    fmap (\c -> show (c ^. chapter) ++ "," ++ show (length $ T.words (c ^. text)))

makePerspectivesCsv :: [Chapter] -> String
makePerspectivesCsv = 
    intercalate "\n" . 
    ("Perspective,Count":) . 
    fmap (\(p, len) -> T.unpack p ++ "," ++ show len) .
    sortBy (\(_, l1) (_, l2) -> compare l2 l1) .
    frequency .
    fmap (^. perspective)

makeWordCountCsv :: [Chapter] -> String
makeWordCountCsv = 
    intercalate "\n" . 
    ("Word,Count":) . 
    fmap (\(p, len) -> T.unpack p ++ "," ++ show len) .
    sortBy (\(_, l1) (_, l2) -> compare l2 l1) .
    M.toList . 
    foldl' frequency' M.empty .
    fmap (T.words . T.map (\case
        c | isAlphaNum c || c == '\'' -> c
        _ -> ' '
    ) . T.toLower . (^. text))
    
frequency' :: (Num n, Ord a) => Map a n -> [a] -> Map a n
frequency' = foldl' (flip $ alter (Just . maybe 1 (+1)))

frequency :: (Num n, Ord a) => [a] -> [(a, n)]
frequency = M.toList . foldl' (flip $ alter (Just . maybe 1 (+1))) M.empty

chapterWordFrequency' :: Map Text Int -> Chapter -> Map Text Int
chapterWordFrequency' m chp = foldl' frequency' m (T.words <$> [chp ^. perspective, chp ^. date, chp ^. text])

chaptersWordFrequency :: [Chapter] -> Map Text Int
chaptersWordFrequency = foldl' chapterWordFrequency' M.empty