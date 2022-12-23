{-# LANGUAGE TemplateHaskell #-}
module Reddit.Types.Lens(module Reddit.Types.Lens) where
import Reddit.Types
import Control.Lens (makeLenses)
makeLenses ''Thing
makeLenses ''Listing
makeLenses ''Link