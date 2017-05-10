{-# LANGUAGE DuplicateRecordFields #-}

module Atlassian.Bitbucket.Server.Types where

import           Data.Aeson
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Servant.API

import           Atlassian.Internal.JSON

--------------------------------------------------------------------------------

data Author = Author
  { user :: User
  } deriving (Generic, Show)

instance ToJSON Author where
  toJSON = genericToJSON defaultCamel

instance FromJSON Author where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

data User = User
  { displayName  :: Text
  , emailAddress :: Text
  } deriving (Generic, Show)

instance ToJSON User where
  toJSON = genericToJSON defaultCamel

instance FromJSON User where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

data PRLink = PRLink
  { linkSelf :: Text
  } deriving (Generic, Show)

instance ToField PRLink where
  toField = toField . linkSelf

instance ToJSON PRLink where
  toJSON = genericToJSON defaultCamel

instance FromJSON PRLink where
  parseJSON x = do
    self <- withObject "links" (.: "self") x
    firstLink <- withArray "self" (return . V.head) self
    actualLink <- withObject "self"  (.: "href") firstLink
    return $ PRLink actualLink

--------------------------------------------------------------------------------

data Reviewer = Reviewer
  { approved :: Bool
  } deriving (Generic, Show)

instance ToField Reviewer where
  toField = toField . approved

instance ToJSON Reviewer where
  toJSON = genericToJSON defaultCamel

instance FromJSON Reviewer where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

data PR = PR
  { title       :: Text
  , id          :: Int
  , description :: Maybe Text
  , createdDate :: Int
  , updatedDate :: Int
  , author      :: Author
  , links       :: PRLink
  , reviewers   :: Vector Reviewer
  , fromRef     :: FromRef
  } deriving (Generic, Show)

instance ToJSON PR where
  toJSON = genericToJSON defaultCamel

instance FromJSON PR where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

newtype FromRef = FromRef
  { latestCommit :: Text
  } deriving (Generic, Show)

instance ToField FromRef where
  toField = toField . latestCommit

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
