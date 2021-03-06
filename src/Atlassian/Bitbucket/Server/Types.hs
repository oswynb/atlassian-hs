{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}

module Atlassian.Bitbucket.Server.Types where

import           Data.Aeson
import           Data.Char                          (toUpper)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Servant.API

import           Atlassian.Internal.JSONx

--------------------------------------------------------------------------------

type Project = Text
type Slug    = Text

--------------------------------------------------------------------------------

data PagedResponse a = PagedResponse
  { size       :: Int
  , limit      :: Int
  , isLastPage :: Bool
  , values     :: [a]
  , start      :: Int
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (PagedResponse a) where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------
newtype Author = Author
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

newtype PRLink = PRLink
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
  { user      :: User
  , status    :: ReviewerStatus
  } deriving (Generic, Show)

instance ToJSON Reviewer where
  toJSON = genericToJSON defaultCamel

instance FromJSON Reviewer where
  parseJSON = genericParseJSON defaultCamel

data ReviewerStatus = Approved
                    | Unapproved
                    | NeedsWork
  deriving (Generic, Eq, Show)

instance ToJSON ReviewerStatus where
  toJSON = genericToJSON defaultCamel

decodeReviewerStatus :: Text -> Maybe ReviewerStatus
decodeReviewerStatus "APPROVED"   = Just Approved
decodeReviewerStatus "UNAPPROVED" = Just Unapproved
decodeReviewerStatus "NEEDS_WORK" = Just NeedsWork
decodeReviewerStatus _            = Nothing

instance FromJSON ReviewerStatus where
  parseJSON = withText "ReviewerStatus" $ \x -> case decodeReviewerStatus x of
    Just x  -> return x
    Nothing -> fail $ "Invalid Reviewer Status: " <> T.unpack x

--------------------------------------------------------------------------------

data PR = PR
  { title       :: Text
  , id          :: Int
  , description :: Maybe Text
  , createdDate :: Int
  , updatedDate :: Int
  , author      :: Author
  , links       :: PRLink
  , reviewers   :: [Reviewer]
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

--------------------------------------------------------------------------------

newtype GetReposResponse = GetReposResponse
  { slug :: Slug
  } deriving (Generic, Show)

instance FromJSON GetReposResponse where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

newtype CommitInfo = CommitInfo
  { id :: Text
  } deriving (Generic, Show)

instance FromJSON CommitInfo where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

data TaskCount = TaskCount
  { open     :: Int
  , resolved :: Int
  } deriving (Generic, Show)

instance FromJSON TaskCount where
  parseJSON = genericParseJSON defaultCamel

--------------------------------------------------------------------------------

data CommitBuildStatus = CommitBuildStatus
  { state :: BuildState
  , url   :: Text
  } deriving (Generic, Show)

instance FromJSON CommitBuildStatus where
  parseJSON = genericParseJSON defaultCamel

data BuildState = Successful
                | Failed
                | InProgress
  deriving (Generic, Eq, Show)

instance ToJSON BuildState where
  toJSON = genericToJSON defaultOptions{constructorTagModifier=map toUpper}

instance FromJSON BuildState where
  parseJSON = genericParseJSON defaultOptions{constructorTagModifier=map toUpper}

--------------------------------------------------------------------------------
