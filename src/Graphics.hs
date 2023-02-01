module Graphics(module Graphics) where
import Lib
import Control.Lens
import Graphics.Rendering.Chart.Easy
import Control.Arrow
import Data.Ratio
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

perspectivesPiChart :: [Chapter] -> Renderable ()
perspectivesPiChart chps = toRenderable (
    pie_title .~ "Chapters By Perspective" $
    pie_plot . pie_data .~ fmap (\(s, v) -> pitem_label .~ s $ pitem_value .~ v $ def) (perspectiveFrequencies chps) $ 
    def)
    where
        perspectiveFrequencies = frequency . fmap (T.unpack . (^. perspective))

perspectivesWordCountPiChart :: [Chapter] -> Renderable ()
perspectivesWordCountPiChart chps = toRenderable (
    pie_title .~ "Number of Words By Perspective" $
    pie_plot . pie_data .~ fmap (\(s, v) -> pitem_label .~ s $ pitem_value .~ fromIntegral v $ def) (wordCountByPerspective chps) $ 
    def)
    where
        getWordCount = length . T.words . (^. text) -- extracts the chapter word count as a string
        wordCountByPerspective = M.toList . groupOnBy (+) . fmap (getPerspective &&& getWordCount)

perspectivesWordAveragePiChart :: [Chapter] -> Renderable ()
perspectivesWordAveragePiChart chps = toRenderable (
    pie_title .~ "Average Words Per Perspective" $
    pie_plot . pie_data .~ fmap (\(s, v) -> pitem_label .~ s $ pitem_value .~ v $ def) (wordCountAverages chps) $ 
    def)
    where
        getWordCountA = makeAverage . length . T.words . (^. text)
        wordCountAverages = M.toList . fmap (fromIntegral . getAverage) . groupOn . fmap (getPerspective &&& getWordCountA)

getPerspective :: Chapter -> String
getPerspective = T.unpack . (^. perspective) -- extracts the chapter perspective as a string

data Average a = Average {
    s :: a,
    n :: a
}

makeAverage :: Num a => a -> Average a
makeAverage i = Average i 1

instance Num a => Semigroup (Average a) where
    Average {s = s1, n = n1} <> Average {s = s2, n = n2} = Average {s = s1 + s2, n = n1 + n2}

instance Num a => Monoid (Average a) where
    mempty = Average 0 0

getAverage :: Integral a => Average a -> a
getAverage (Average {s = s, n = n}) = s `div` n

getAverageTrun :: Integral a => Average a -> a
getAverageTrun a = round $ getAverageR a

getAverageR :: Integral a => Average a -> Ratio a
getAverageR (Average {s = s, n = n}) = s % n
