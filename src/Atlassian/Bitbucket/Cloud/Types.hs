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
  deriving (Generic, Show)

iso8601FormatString :: String
iso8601FormatString = iso8601DateFormat (Just "%H:%M:%S.%QZ")

instance FromJSON ISO8601 where
  parseJSON = withText "iso8601" $ \x ->
    let parsed = parseTimeM False defaultTimeLocale iso8601FormatString (T.unpack x)
    in case parsed of
      Nothing -> fail "Unable to parse string to iso8601"
      Just t  -> return $ ISO8601 t

--------------------------------------------------------------------------------

data PRLink = PRLink
  { html :: Text
  } deriving (Generic, Show)

instance FromJSON PRLink where
  parseJSON x = do
    _html <- withObject "links" (.: "html") x
    actualLink <- withObject "html"  (.: "href") _html
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

data SimplePipelineState = Pending
                         | Successful
                         | Failed
                         | Error
  deriving (Generic, Show)

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

data PipelineResult = PipelineResult
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

data GetPipelinesResponse = GetPipelinesResponse
  { uuid        :: Text
  , completedOn :: Maybe ISO8601
  , createdOn   :: ISO8601
  , state       :: PipelineState
  , refName     :: Text -- Generally the branch name
  } deriving (Generic, Show)

instance FromJSON GetPipelinesResponse where
  parseJSON = genericParseJSON defaultSnake

--------------------------------------------------------------------------------
