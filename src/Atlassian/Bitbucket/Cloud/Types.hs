{-# LANGUAGE DuplicateRecordFields #-}

module Atlassian.Bitbucket.Cloud.Types where

import           Data.Aeson
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           GHC.Generics
import           Servant.API

import           Atlassian.Internal.JSON

--------------------------------------------------------------------------------

data PRLink = PRLink
  { html :: Text
  } deriving (Generic, Show)

instance FromJSON PRLink where
  parseJSON x = do
    html <- withObject "links" (.: "html") x
    actualLink <- withObject "html"  (.: "href") html
    return $ PRLink actualLink

--------------------------------------------------------------------------------

data Reviewer = Reviewer
  { reviewerApproved :: Bool
  } deriving (Generic, Show)

instance ToJSON Reviewer where
  toJSON = genericToJSON defaultCamel

instance FromJSON Reviewer where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

newtype FromRef = FromRef
  { latestCommit :: Text
  } deriving (Generic, Show)

instance ToJSON FromRef where
  toJSON = genericToJSON defaultCamel

instance FromJSON FromRef where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

data PRState = Open
             | Declined
             | Merged
  deriving (Eq)

instance Show PRState where
  show Open     = "OPEN"
  show Declined = "DECLINED"
  show Merged   = "MERGED"

instance ToHttpApiData PRState where
  toQueryParam = T.pack . show

--------------------------------------------------------------------------------

newtype Owner = Owner Text
  deriving (Eq, Generic, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype Slug = Slug Text
  deriving (Eq, Generic, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

--------------------------------------------------------------------------------

data GetRepositoriesResponse = GetRepositoriesResponse {
  fullName :: Text
} deriving (Generic, Show)

instance FromJSON GetRepositoriesResponse where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

data PR = PR
  { author       :: Maybe Author
  , title        :: Text
  , id           :: Int
  , taskCount    :: Maybe Int
  , links        :: PRLink
  , participants :: Maybe [Participant]
  } deriving (Generic, Show)

instance FromJSON PR where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

data Participant = Participant
  { approved :: Bool
  } deriving (Generic, Show)

instance FromJSON Participant where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

data Author = Author
  { displayName :: Text
  } deriving (Generic, Show)

instance FromJSON Author where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

data PagedResponse a = PagedResponse
  { size     :: Maybe Int
  , page     :: Maybe Int
  , pagelen  :: Int
  , next     :: Maybe Text
  , previous :: Maybe Text
  , values   :: [a]
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (PagedResponse a) where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------
