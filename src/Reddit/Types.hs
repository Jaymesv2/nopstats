{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reddit.Types(module Reddit.Types) where
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

data Thing a = Thing { 
    _id' :: Maybe Text,
    _name' :: Maybe Text,
    _kind' :: Text,
    _data' :: a
} deriving (Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1 . init} ''Thing)
makeLenses ''Thing

data Listing a = Listing{ 
    _before :: Maybe Text,
    _after :: Maybe Text,
    _modhash :: Text,
    _children :: [Thing a]
} deriving (Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Listing)
makeLenses ''Listing

{-
class Votable a where
    ups :: a -> Int
    downs :: a -> Int
    likes :: a -> Maybe Int

class Created a where
    created :: a -> Int
    created_utc :: a -> Int

data Votable = Votable { 
    _ups :: Int,
    _downs :: Int,
    _likes :: Bool
} deriving (Generic, Show)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Votable)

data Created = Created { 
	_created :: Int,
    _created_utc :: Int
} deriving (Generic, Show)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Created)
-}

data Award = Award { 
    _award_type :: Text,
    _award_sub_type :: Text,
    _name :: Text,
    _description :: Text,
    _id :: Text,
    _is_enabled :: Bool,
    _is_new :: Bool,
    _coin_price :: Int,
    _coin_reward :: Int,
    _count :: Int
} deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Award)
makeLenses ''Award

data Link = Link { 
    _author_flair_text :: Maybe Text,
    _all_awardings :: Maybe [Award],
    _domain :: Maybe Text,
    _clicked :: Bool,
    _media :: Maybe Object,
    _num_comments :: Int,
    _author :: Text,
    _score :: Int,
    _title :: Text,
    _subreddit :: Maybe Text,
    _thumbnail :: Text,
    _url :: Text,
    -- _edited :: Maybe Double, -- not sure why this doesnt work
    _distinguished :: Maybe Text,
    _selftext :: Text,
    _over_18 :: Bool,
    _permalink :: Text,
    _selftext_html :: Text,

    -- i want to make an optic for these
    _ups :: Int,
    _downs :: Int,
    _created :: Int,
    _created_utc :: Int
} deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Link)
makeLenses ''Link