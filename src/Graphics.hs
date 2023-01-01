module Graphics(module Graphics) where
import Lib
import Data.Text
import Control.Lens
import Graphics.Rendering.Chart.Easy

perspectivesPiChart :: [Chapter] -> Renderable ()
perspectivesPiChart chps = toRenderable (
    pie_title .~ "Chapters By Perspective" $
    pie_plot . pie_data .~ fmap (\(s, v) -> pitem_label .~ s $ pitem_value .~ v $ def) (frequency $ unpack . (^. perspective) <$> chps) $ 
    def)