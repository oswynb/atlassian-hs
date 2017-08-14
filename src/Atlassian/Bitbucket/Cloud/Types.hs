{-# LANGUAGE DuplicateRecordFields #-}

module Atlassian.Bitbucket.Cloud.Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           GHC.Generics
import           Servant.API

import           Atlassian.Internal.JSON

--------------------------------------------------------------------------------

type Owner = Text
type Slug  = Text

newtype ISO8601 = ISO8601 { unISO8601 :: UTCTime }
  deriving (Generic, Eq, Ord, Show)

iso8601FormatString :: String
iso8601FormatString = iso8601DateFormat (Just "%H:%M:%S%QZ")

instance FromJSON ISO8601 where
  parseJSON = withText "iso8601" $ \x ->
    let parsed = parseTimeM False defaultTimeLocale iso8601FormatString (T.unpack x)
    in case parsed of
      Nothing -> fail "Unable to parse string to iso8601"
      Just t  -> return $ ISO8601 t

--------------------------------------------------------------------------------

newtype PRLink = PRLink
  { html :: Text
  } deriving (Generic, Show)

instance FromJSON PRLink where
  parseJSON x = do
    _html <- withObject "links" (.: "html") x
    actualLink <- withObject "html"  (.: "href") _html
    return $ PRLink actualLink

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

newtype GetRepositoriesResponse = GetRepositoriesResponse {
  fullName :: Text
} deriving (Generic, Show)

instance FromJSON GetRepositoriesResponse where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

newtype PRSource = PRSource
  { branch :: Maybe SourceBranch
  } deriving (Generic, Show)

instance FromJSON PRSource where
  parseJSON = genericParseJSON defaultSnake

newtype SourceBranch = SourceBranch
  { name :: Text
  } deriving (Generic, Show)

instance FromJSON SourceBranch where
  parseJSON = genericParseJSON defaultSnake

data PR = PR
  { author       :: Maybe User
  , title        :: Text
  , id           :: Int
  , taskCount    :: Maybe Int
  , links        :: PRLink
  , participants :: Maybe [Participant]
  , source       :: PRSource
  , description  :: Text
  } deriving (Generic, Show)

instance FromJSON PR where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

data Participant = Participant
  { user     :: User
  , approved :: Bool
  } deriving (Generic, Show)

instance FromJSON Participant where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

data User = User
  { displayName :: Text
  , links       :: UserLinks
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON defaultSnake

newtype UserLinks = UserLinks
  { avatar :: HrefLink
  } deriving (Generic, Show)

instance FromJSON UserLinks where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------

newtype HrefLink = HrefLink
  { href :: Text
  } deriving (Generic, Show)

instance FromJSON HrefLink where
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

data SimplePipelineState = Pending
                         | Successful
                         | Failed
                         | Error
  deriving (Generic, Show, Eq)

data PipelineState = PipelineState
  { _type   :: PipelineStateType
  , _result :: Maybe PipelineResult -- Exists for completed pipelines
  } deriving (Generic, Show)

instance FromJSON PipelineState where
  parseJSON = genericParseJSON $ (aesonDrop 1 snakeCase){omitNothingFields = True}

data PipelineStateType = PipelineStateCompleted
                       | PipelineStatePending
  deriving (Generic, Show)

instance FromJSON PipelineStateType where
  parseJSON = genericParseJSON $ (aesonDrop 1 snakeCase){constructorTagModifier = snakeCase}

newtype PipelineResult = PipelineResult
  { _type :: PipelineResultType
  } deriving (Generic, Show)

instance FromJSON PipelineResult where
  parseJSON = genericParseJSON $ aesonDrop 1 snakeCase

data PipelineResultType = PipelineStateCompletedSuccessful
                        | PipelineStateCompletedFailed
                        | PipelineStateCompletedError
  deriving (Generic, Show)

instance FromJSON PipelineResultType where
  parseJSON = genericParseJSON $ (aesonDrop 1 snakeCase){constructorTagModifier = snakeCase}

newtype PipelineTarget = PipelineTarget
  { refName :: Text -- Generally the branch name
  } deriving (Generic, Show)

instance FromJSON PipelineTarget where
  parseJSON = genericParseJSON defaultSnake

data GetPipelinesResponse = GetPipelinesResponse
  { uuid        :: Text
  , completedOn :: Maybe ISO8601
  , createdOn   :: ISO8601
  , state       :: PipelineState
  , target      :: PipelineTarget
  } deriving (Generic, Show)

instance FromJSON GetPipelinesResponse where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------
